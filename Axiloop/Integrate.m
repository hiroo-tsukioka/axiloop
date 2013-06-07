(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2013 Oleksandr Gituliar.                               *)
(*                                                                            *)
(*  This file is part of Axiloop.                                             *)
(*                                                                            *)
(*  Axiloop is free software: you can redistribute it and/or modify           *)
(*  it under the terms of the GNU General Public License as published by      *)
(*  the Free Software Foundation, either version 3 of the License, or         *)
(*  (at your option) any later version.                                       *)
(*                                                                            *)
(*  This program is distributed in the hope that it will be useful,           *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *)
(*  GNU General Public License for more details.                              *)
(*                                                                            *)
(*  You should have received a copy of the GNU General Public License         *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                            *)
(*============================================================================*)


BeginPackage["Axiloop`Integrate`", {
	"Axiloop`Core`",
	"Axiloop`Exception`",
	"Axiloop`Tracer`"}
]

$$::usage = ""

eir::usage =
	"IR pole in 4 - 2 eta dimensions."

euv::usage =
	"UV pole in 4 - 2 eta dimensions."

l::usage =
	"Loop momentum."

IntegrateLoop::usage =
	"Integrate over loop momenta."

I0::usage =
	"Principal Value regulated integral; I0 = - Log[delta] + O[delta]."

I1::usage =
	"Principal Value regulated integral; I1 = - (Log[delta]^2)/2 - Li2[1]/4 + O[delta]."

Li2::usage =
	"Dilogarythm function; Li2[x] = - Integrate[Log[1-t]/t, {t, 0, x}]."

B0;B1;B3;C0;C1;C3;D0;E0;E1;E2;E3;K0;P0;P1;P3;R0;R1;R2;R3;R4;R5;R6;S0;S1;S2;T0;T1;V0;V1;V2;U0;


Begin["`Private`"] 

$$CollectLoopIntegrals::unevaluated = "`1`";

$$CollectLoopIntegrals[expr_, l_] := Module[
	{collectRules, result},
	
	collectRules = {
		$$[{a___},{b___},{c___}] S[l, x_] :> $$[{a,x},{b},{c}]
		,
		$$[{a___},{b___},{c___}] S[l, x_]^n_ :>
			$$[Flatten[{a,x&/@Range[n]}],{b},{c}] /; n>0
		,

		$$[{a___},{b___},{c___}] / S[l, l] :> $$[{a},{b,0},{c}]
		,
		$$[{a___},{b___},{c___}] / S[l+x_Symbol, l+x_Symbol] :>
			$$[{a},{b,x},{c}]
		,
		$$[{a___},{b___},{c___}] / S[l-x_Symbol, l-x_Symbol] :>
			$$[{a},{b,-x},{c}]
		,

		$$[{a___},{b___},{c___}] S[l, n]^-1 :> $$[{a},{b},{c,0}]
		,
		$$[{a___},{b___},{c___}] S[l+d_, n]^-1 :> $$[{a},{b},{c,d}]
		,
		$$[{a___},{b___},{c___}] S[l-d_, n]^-1 :> $$[{a},{b},{c,-d}]
		,
		$$[{a___},{b___},{c___}] S[-l+d_, n]^-1 :> - $$[{a},{b},{c,-d}]
		,
		
		$$[{},{},{}] -> 1
	};
	
	result = Expand[expr $$[{},{},{}]]
		//. collectRules
		/. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]
	];
	If[
		!(FreeQ[result, Dot[l,_]] && FreeQ[result, Dot[_,l]])
		,
		Message[
			$$CollectLoopIntegrals::unevaluated,
			result
		];
		Raise[$UnevaluatedError];
	];

	DEBUG[
		"$$CollectLoopIntegrals"
		,
		ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];

	result
];


$$ExpandLoopIntegrals[expr_, l_] := Module[
	{expandRules},
	
	expandRules = {
		$$[{x_,a___},{b___},{c___}] :> $$[{a},{b},{c}] l.x,
		$$[{a___},{x_,b___},{c___}] :> $$[{a},{b},{c}] / (l+x).(l+x),
		$$[{a___},{b___},{x_,c___}] :> $$[{a},{b},{c}] / (l+x).n,
		$$[{},{},{}] -> 1
	};
	
	Expand[expr //. expandRules]
];


$$SimplifyAlgebraic[expr_] := Module[
	{result, signRules, simplifyRules},
		
	signRules = {
		$$[{a___}, {b___}, {c___}] :>
			(-1)^(Length[Select[{a}, !MatchQ[#,l] &]]+Length[{c}]) $$[{a}, -# &/@ {b}, -# &/@ {c}] /;
				{b} == Select[{b}, Or[# == 0, MatchQ[#, Times[-1, _Symbol]]] &]
	};

	simplifyRules = {
		$$[{x_,y_}, {0,q}, {}] :>
			$$[{x,y}, {k,p}, {}] + k.x k.y $$[{}, {k,p}, {}]
			+ k.x $$[{y}, {k,p}, {}] + k.y $$[{x}, {k,p}, {}] 
		,

		$$[{k}, {0,k,p}, {c_}] :> 1/2 (
			$$[{}, {0,p}, {c}] - $$[{}, {k,p}, {c}] - k.k $$[{}, {0,k,p}, {c}]
		)
		,
		$$[{p}, {0,k,p}, {c_}] :> 1/2 (
			$$[{}, {0,k}, {c}] - $$[{}, {k,p}, {c}] - p.p $$[{}, {0,k,p}, {c}]
		)
		,
		
		$$[{p,p}, {0,k,p}, {c_}] :> 1/2 (
			$$[{p}, {0,k}, {c}] - $$[{p}, {k,p}, {c}] - p.p $$[{p}, {0,k,p}, {c}]
		)
		,
		$$[{k,k}, {0,k,p}, {c_}] :> 1/2 (
			$$[{k}, {0,p}, {c}] - $$[{k}, {k,p}, {c}] - k.k $$[{k}, {0,k,p}, {c}]
		)
		,
		$$[{k,p}, {0,k,p}, {c_}] :> 1/2 (
			$$[{k}, {0,k}, {c}] - $$[{k}, {k,p}, {c}] - p.p $$[{k}, {0,k,p}, {c}]
		)
		,
		
		(* l.n terms in the numerator *)
		$$[{n}, b_, {x_}] :>
			$$[{}, b, {}] - x.n $$[{}, b, {x}]
		,
		$$[{n,a__}, b_, {x_}] :>
			$$[{a}, b, {}] - x.n $$[{a}, b, {x}]
		,
		$$[{a__,n}, b_, {x_}] :>
			$$[{a}, b, {}] - x.n $$[{a}, b, {x}]
		,
		
		(* 1/(l.n-x.n) 1/(l.n-y.n) *)
		$$[{a___}, {b___}, {x_,y_,c___}] :>
			($$[{a}, {b}, {y,c}] - $$[{a}, {b}, {x,c}]) / (x.n-y.n)
		,
		$$[x__, {k,p}] :> ($$[x, {k}] - $$[x, {p}]) / (p.n-k.n)
		,
		
		(* l.l terms in the numerator *)
		$$[{l}, {k,p}, {c_}] :>
			$$[{}, {p}, {c}] - 2 $$[{k}, {k,p}, {c}] - k.k $$[{}, {k,p}, {c}]
	};
		
	result = Expand[expr /. signRules //. simplifyRules];
	
	DEBUG[
		"$$SimplifyAlgebraic"
		,
		ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];
	
	result
];


$$SimplifyOnePoint[expr_] := Module[
	{simplifyRules},
	
	simplifyRules = {
		$$[{}, {k}, {0}] ->
			$$[{}, {0}, {0}] - 2 $$[{k}, {0,k}, {0}] - k.k $$[{}, {0,k}, {0}]
		,
		$$[{}, {q}, {0}] ->
			$$[{}, {0}, {0}] - 2 $$[{q}, {0,q}, {0}] - q.q $$[{}, {0,q}, {0}]
	};
	
	Expand[expr /. simplifyRules]
]


$$SimplifyTranslate[expr_] := Module[
	{result, simplifyRules},
	
	simplifyRules = {
		$$[{}, {q}, {}] -> $$[{}, {0}, {}],
		
		$$[{}, {p}, {p}] -> $$[{}, {0}, {0}]
		,
		$$[{}, {p}, {k}] -> $$[{}, {q}, {0}]
		,
		
		$$[{ },{0,k},{k}] -> - $$[{ },{0, k},{0}]
		,
		$$[{ },{0,k},{p}] -> - $$[{ },{p, q},{0}]
		,
		$$[{ },{0,p},{k}] -> - $$[{ },{k,-q},{0}]
		,
		$$[{ },{0,p},{p}] -> - $$[{ },{0, p},{0}]
		,
		$$[{ },{k,p},{k}] ->   $$[{ },{0, q},{0}],
		$$[{ },{k,p},{p}] -> - $$[{ },{0, q},{0}],
		
		$$[{k},{0,k},{k}] ->   $$[{k},{0, k},{0}] + k.k $$[{},{0,k},{0}],
		$$[{k},{0,k},{p}] ->   $$[{k},{p, q},{0}] + k.p $$[{},{p,q},{0}],
		$$[{p},{0,k},{p}] ->   $$[{p},{p, q},{0}] + p.p $$[{},{p,q},{0}],
		$$[{k},{0,p},{p}] ->   $$[{k},{0, p},{0}] + k.p $$[{},{0,p},{0}],
		$$[{k},{k,p},{k}] ->   $$[{k},{0, q},{0}] - k.k $$[{},{0,q},{0}],
		$$[{k},{k,p},{p}] ->   $$[{k},{0, q},{0}] + k.p $$[{},{0,q},{0}],
		$$[{p},{0,k},{k}] ->   $$[{p},{0, k},{0}] + k.p $$[{},{0,k},{0}],
		$$[{p},{k,p},{k}] ->   $$[{p},{0, q},{0}] - k.p $$[{},{0,q},{0}],
		$$[{p},{k,p},{p}] ->   $$[{p},{0, q},{0}] + p.p $$[{},{0,q},{0}],
		
		$$[{},{0,k,p},{k}] -> - $$[{},{0,k,-q},{0}],
		$$[{},{0,k,p},{p}] -> - $$[{},{0,p, q},{0}]
	};
	
	result = Simplify[expr /. simplifyRules];
	
	DEBUG[
		"$$SimplifyTranslate"
		,
		ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];
	
	result
];


$$ExpandPV[expr_] := Module[
	{expandRules},
	
	expandRules = {
		B0 -> I0/euv - I1 + Li2[1],
		B1 -> 1/euv + 2,
		B3 -> (I0 - 2)/euv - I1 - 4 + Li2[1],

		C0 -> (I0 + Log[1-x])/euv - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1],
		C1 -> 1/euv + 2,
		C3 -> (I0 + Log[1-x] - 2)/euv - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 - 4
				+ Li2[1],

		D0 -> (Log[1-x] - Log[x])/euv + (Log[x]^2)/2 - (Log[1-x]^2)/2 + Li2[1]
				- 2 Li2[1-x] - Log[x]Log[1-x],

		E0 -> - Log[1-x]/(x euv),
		E1 -> - 1/euv Log[1-x]/x - (2 Li2[x] + (Log[1-x]^2)/2)/x,
		E2 ->   1/euv (x + Log[1-x])/x^2 + (2 Li2[x] + (Log[1-x]^2)/2 - 2x)/x^2,
		E3 ->   1/euv ((x-2)Log[1-x] - 2x)/x^3 + (4x + (x-2)(2 Li2[x] + (Log[1-x]^2)/2))/x^3,

		K0 -> - Log[x]/((1-x) euv),

		P0 -> (I0 + Log[x])/euv - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
		P1 -> 1/euv + 2,
		P3 -> (I0 + Log[x] - 2)/euv - I1 + I0 Log[x] + (Log[x]^2)/2 - 4
				+ Li2[1],
		
		U0 -> - ((3 I0 + 3 Log[1-x] - Log[x])/eir - 5 I1 + 2 I0 Log[1-x] - (Log[1-x]^2)/2 + I0 Log[x] + (Log[x]^2)/2 - 2 Li2[1-x] + 5 Li2[1]),

		R0 -> - ((2 I0 + Log[1-x])/eir - 4 I1 + 2 I0 Log[1-x] + (Log[1-x]^2)/2),
		R1 ->   R0 + 2/eir + 4,
		R2 -> - 1/eir - 2,
		R3 ->   R0 + 3/eir + 7,
		R4 -> - 1/(2 eir) - 1,
		R5 -> - 1/(2 eir) - 3/2,
		R6 ->   1/(4 euv) + 3/4,
		
		S0 -> - ((3 I0 + Log[1-x] - Log[x]) / eir - 5 I1 + 2 I0 Log[1-x] + I0 Log[x] + (Log[x]^2)/2  + (Log[1-x]^2)/2 + 2 Li2[1-x] + Li2[1]),
		(*
		R0 ->  1/eir^2 - Li2[1],
		R1 ->  1/eir^2 + 2/eir + 4 - Li2[1],
		R2 -> -1/eir - 2,
		R3 ->  1/eir^2 + 3/eir + 7 - Li2[1],
		R4 -> -1/(2 eir) - 1,
		R5 -> -1/(2 eir) - 3/2,
		R6 ->  1/(4 euv) + 3/4,
		
		U0 -> 1/eir^2 + (-I0 + Log[x] - 2 Log[1-x])/eir + I1 -I0 Log[x] + 2 Li2[1-x] - (Log[x]^2)/2 + Log[1-x]^2 - 6 Li2[1]

		S0 -> 1/eir^2 - (I0 - Log[x])/eir + I1 - I0 Log[x] - 2 Li2[1]
				- 2 Li2[1-x] - (Log[x]^2)/2,
		*)
		S1 -> 1/eir^2 - x Log[x]/((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
		S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x),

		T0 -> 1/euv + 2,
		T1 -> 1/(2 euv) + 1,

		V0 -> Ln[x],
		V1 -> (1-x + x Log[x])/(1-x)^2 / euv,
		V2 -> - (1-x + Log[x])/(1-x)^2 / euv
	};
	
	Expand[expr //. expandRules]
]


IntegrateLoopGeneral::unevaluated = "`1`"

IntegrateLoopGeneral[expr_, l_] := Module[
	{integrateRules, psRule, result, unevaluated},
	
	integrateRules = {
		(*
		$$[{},{0},{ }] -> 0,
		*)
		$$[{},{0},{0}] -> 0,
		
		$$[{},{0,k},{ }] ->   Q (k.k)^(-eir) T0,
		$$[{},{0,p},{ }] ->   Q (p.p)^(-eir) T0,
		$$[{},{k,p},{ }] ->   Q (q.q)^(-eir) T0,

		$$[{x_},{0,k},{}] :> - Q (k.k)^(-eir) k.x T1,
		$$[{x_},{0,p},{}] :> - Q (p.p)^(-eir) p.x T1,
		$$[{x_},{0,q},{}] -> - Q (q.q)^(-eir) q.x T1,
		$$[{x_},{k,p},{}] :> - Q (q.q)^(-eir) (q.x T1 - k.x T0),

		$$[{},{0,k,p},{}] -> Q (k.k)^(-1-eir) R0,
		$$[{x_},{0,k,p},{}] :> - Q (k.k)^(-1-eir) (p.x R1 + k.x R2),
		$$[{x_, y_},{0,k,p},{}] :> Q (k.k)^(-1-eir) (
    		p.x p.y R3 + k.x k.y R4 + (k.x p.y + p.x k.y) R5 + k.k x.y R6
   		),
   

		$$[{},{0, k},{0}] -> - Q (k.k)^(-eir) P0 / k.n,
		$$[{},{0, p},{0}] -> - Q (p.p)^(-eir) B0 / n.p,
		$$[{},{0, q},{0}] -> - Q (q.q)^(-eir) C0 / n.q,
		$$[{},{k, p},{0}] -> - Q (q.q)^(-eir) K0 / n.p,
		$$[{},{k,-q},{0}] ->   Q (p.p)^(-eir) D0 / n.p,
		$$[{},{p, q},{0}] -> - Q (k.k)^(-eir) E0 / n.p,
		
		$$[{x_},{0,k},{0}] :> Q (k.k)^(-eir)/k.n (
			k.x P1 + n.x k.k/(2 k.n) P3
		),
		$$[{x_},{0,p},{0}] :> Q (p.p)^(-eir)/p.n (
			p.x B1 + n.x p.p/(2 p.n) B3
		),
		$$[{x_},{0,q},{0}] :> Q (q.q)^(-eir)/q.n (
			q.x C1 + n.x q.q/(2 q.n) C3
		),
		$$[{x_},{k,p},{0}] :> Q (q.q)^(-eir) (
			p.x V1 + k.x V2 + n.x q.q V3
		),
		$$[{x_},{p,q},{0}] :> Q (k.k)^(-eir)/p.n (
			p.x E1 + k.x E2 + n.x k.k/2 E3
		),
		
		$$[{},{0,k, p},{0}] -> - Q (k.k)^(-1-eir)/n.p S0,
		$$[{},{0,k,-q},{0}] -> Q (k.k)^(-1-eir)/q.n U0,
		
		$$[{},{0,p, q},{0}] -> - Q (k.k)^(-1-eir)/k.n V0,
		
	
		$$[{x_},{0,k,p},{0}] :> Q (k.k)^(-1-eir)/p.n (
			p.x S1 + k.x S2 + n.x k.k/(2 k.n) S3
		)
	};
	
	psRule = {
		Q -> I (4 Pi)^(-2+eir) Gamma[1+eir]
	};

	result = Expand[expr /. integrateRules /. psRule];
	
	unevaluated = Union[Cases[result, $$[__], {0, Infinity}]];
	If[
		unevaluated != {}
		,
		Message[
			IntegrateLoopGeneral::unevaluated,
			ToString[#] &/@ unevaluated
		];
		Raise[$UnevaluatedError];
	];
	
	result
];


IntegrateLoop[expr_, l_] := Module[
	{collected, integrated, integratedPV, simplified},
	
	collected = $$CollectLoopIntegrals[expr, l];
	
	simplified = collected;
	simplified = $$SimplifyAlgebraic[simplified];
	simplified = $$SimplifyTranslate[simplified];
	simplified = $$SimplifyOnePoint[simplified];
	
	integrated = Expand[
		IntegrateLoopGeneral[simplified, l]
			/. $kinematicRules
	];

	integratedPV = $$ExpandPV[integrated];

	{
		{"collected", collected},
		{"simplified", simplified},
		{"integrated", {
			{"short", integrated},
			{"long", integratedPV}}}
	}
];

End[]


EndPackage[]
