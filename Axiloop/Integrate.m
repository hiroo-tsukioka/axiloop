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
	
PV::usage = ""

Q;X0;Y0;B0;B1;B3;C0;C1;C3;D0;E0;E1;E2;E3;H1;H2;H3;H4;K0;P0;P1;P3;
R0;R1;R2;R3;R4;R5;R6;
H0;H1;H2;H3;H4;H5;H6;
S0;S1;S2;S3;S4;S5;S6;S7;S8;S9;S10;
U0;U1;U2;U3;U4;U5;U6;U7;U8;U9;U10;
W0;W1;W2;W3;W4;W5;W6;W7;W8;W9;W10;
T0;T1;T2;T3;V0;V1;V2;U0;


PaVeReduce::usage = ""


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
	
	result = result /. {p.p -> 0, q.q -> 0};
	
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


$$CollectLoopIntegrals[expr_, {l1_, l2_}] := Module[
	{collectRules, result, seed},
	
	collectRules = {
		$$[{{a___}, bc__}, def__] S[l1, x_] :>
			$$[{{a,x}, bc}, def]
		,
		$$[{{a___}, bc__}, def__] S[l1, x_]^n_ :>
			$$[{Flatten[{a,x&/@Range[n]}], bc}, def] /; n>0
		,
		
		$$[{{a___},{b___},{c___}}, def__] / S[l1, l1] :>
			$$[{{a},{b,0},{c}}, def]
		,
		$$[{{a___},{b___},{c___}}, def__] / S[l1, l1]^2 :>
			$$[{{a},{b,0,0},{c}}, def]
		,

		$$[{{a___},{b___},{c___}}, def__] / S[l1+x_Symbol, l1+x_Symbol] :>
			$$[{{a},{b,x},{c}}, def]
		,
		$$[{{a___},{b___},{c___}}, def__] / S[l1-x_Symbol, l1-x_Symbol] :>
			$$[{{a},{b,-x},{c}}, def]
		,

		$$[{{a___},{b___},{c___}}, def__] PV[S[l1, n]]^-1 :>
			$$[{{a},{b},{c,0}}, def]
		,
		$$[{{a___},{b___},{c___}}, def__] PV[S[l1, n]]^-2 :>
			$$[{{a},{b},{c,0,0}}, def]
		,
		$$[{{a___},{b___},{c___}}, def__] PV[S[l1+d_, n]]^-1 :>
			$$[{{a},{b},{c,d}}, def]
		,
		$$[{{a___},{b___},{c___}}, def__] PV[S[l1-d_, n]]^-1 :>
			$$[{{a},{b},{c,-d}}, def]
		,
		$$[{{a___},{b___},{c___}}, def__] PV[S[-l1+d_, n]]^-1 :>
			- $$[{{a},{b},{c,-d}}, def],
			
			
		$$[abc__, {{d___}, ef__}] S[l2, x_] :>
			$$[abc, {{d,x}, ef}]
		,
		$$[abc__, {{d___}, {e___}, {f___}}] / S[l2, l2] :>
			$$[abc, {{d}, {e,0}, {f}}]
		,
		$$[abc__, {{d___}, {e___}, {f___}}] / PV[S[l2, n]] :>
			$$[abc, {{d}, {e}, {f,0}}]
		,
		$$[abc__, {{d___}, {e___}, {f___}}] / PV[S[l2+x_, n]] :>
			$$[abc, {{d}, {e}, {f,x}}]
	};
	
	seed = $$[{{},{},{}}, {{},{},{}}];
	
	result = Expand[expr seed] //. collectRules;
	result = result	/. $$[{{a___},{b___},{c___}}, {{d___},{e___},{f___}}] :>
			$$[{Sort[{a}], Sort[{b}], Sort[{c}]}, {Sort[{d}], Sort[{e}], Sort[{f}]}];
	result = result /. {p.p -> 0, q.q -> 0};
	
	If[
		!(FreeQ[result, Dot[l1,_]] && FreeQ[result, Dot[_,l1]] &&
		  FreeQ[result, Dot[l2,_]] && FreeQ[result, Dot[_,l2]])
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
		$$[{a1___,q,a2___}, {b__}, {c___}] :> $$[Sort[{a1,p,a2}], {b}, {c}] - $$[Sort[{a1,k,a2}], {b}, {c}]
		,

		(* 1/(l.n-x.n) 1/(l.n-y.n) *)
		$$[{a___}, {b___}, {x_,y_,c___}] :>
			($$[{a}, {b}, {y,c}] - $$[{a}, {b}, {x,c}]) / (x.n-y.n)
		,
		$$[x__, {k,p}] :> ($$[x, {k}] - $$[x, {p}]) / (p.n-k.n)
		,
		(*
		$$[{p}, {0,k,p}, {c___}] :> 1/2 (
			$$[{}, {0,k}, {c}] - $$[{}, {k,p}, {c}] - p.p $$[{}, {0,k,p}, {c}]
		),
		$$[{k}, {0,k,p}, {c___}] :> 1/2 (
			$$[{}, {0,p}, {c}] - $$[{}, {k,p}, {c}] - k.k $$[{}, {0,k,p}, {c}]
		)
		,
		
		$$[{a1_,p}, {0,k,p}, {c_}] :> 1/2 (
			$$[{a1}, {0,k}, {c}] - $$[{a1}, {k,p}, {c}] - p.p $$[{a1}, {0,k,p}, {c}]
		),
		$$[{k,a1_}, {0,k,p}, {c_}] :> 1/2 (
			$$[{a1}, {0,p}, {c}] - $$[{a1}, {k,p}, {c}] - k.k $$[{a1}, {0,k,p}, {c}]
		)
		,
		
		$$[{k,a1_,a2_}, {0,k,p}, {c_}] :> 1/2 (
			$$[{a1,a2}, {0,p}, {c}] - $$[{a1,a2}, {k,p}, {c}] - k.k $$[{a1,a2}, {0,k,p}, {c}]
		),
		$$[{a1_,a2_,p}, {0,k,p}, {c_}] :> 1/2 (
			$$[{a1,a2}, {0,k}, {c}] - $$[{a1,a2}, {k,p}, {c}] - p.p $$[{a1,a2}, {0,k,p}, {c}]
		),
		
		
		$$[{k,a1_,a2_}, {0,k,p}, {c_}] :> 1/2 (
			$$[{a1,a2}, {0,p}, {c}] - $$[{a1,a2}, {k,p}, {c}] - k.k $$[{a1,a2}, {0,k,p}, {c}]
		)
		,
		*)
		(* l.n terms in the numerator *)
		$$[{n}, b_, {x_}] :>
			$$[{}, b, {}] - x.n $$[{}, b, {x}]
		,
		$$[{a1___,n,a2___}, b_, {x_}] :>
			$$[{a1,a2}, b, {}] - x.n $$[{a1,a2}, b, {x}]
		,
		(*
		$$[{n,a__}, b_, {x_}] :>
			$$[{a}, b, {}] - x.n $$[{a}, b, {x}]
		,
		$$[{a1___,n,a2___}, b_, {x_}] :>
			$$[{a1,a2}, b, {}] - x.n $$[{a1,a2}, b, {x}]
		,
		
		$$[{a1_,n,a2_}, {0,k,p}, {c_}] :>
			$$[{a1,a2}, {0,k,p}, {}] - c.n $$[{a1,a2}, {0,k,p}, {c}]
		,
		*)
		
		(* l.l terms in the numerator *)
		$$[{l}, {k,p}, {c_}] :>
			$$[{}, {p}, {c}] - 2 $$[{k}, {k,p}, {c}] - k.k $$[{}, {k,p}, {c}]
	};
	
	result = expr /. signRules //. simplifyRules;
	result = result
			/. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]];
	
	DEBUG[
		"$$SimplifyAlgebraic"
		,
		ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];
	
	Expand[result]
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
		$$[{}, {q}, {}] -> $$[{}, {0}, {}]
		,
		$$[{x_,y_},{k,p},{}] :>
			$$[{x,y},{0,q},{}] - k.y $$[{x},{0,q},{}]
			- k.x $$[{y},{0,q},{}] + k.x k.y $$[{},{0,q},{}]
		,
		
		
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
		
		$$[{k},{0,k},{k}] ->   $$[{k},{0, k},{0}] + k.k $$[{},{0, k},{0}],
		$$[{k},{0,k},{p}] ->   $$[{k},{p, q},{0}] + k.p $$[{},{p, q},{0}],
		$$[{k},{0,p},{k}] ->   $$[{k},{k,-q},{0}] + k.k $$[{},{k,-q},{0}],
		$$[{k},{0,p},{p}] ->   $$[{k},{0, p},{0}] + k.p $$[{},{0, p},{0}],
		$$[{k},{k,p},{k}] ->   $$[{k},{0, q},{0}] - k.k $$[{},{0, q},{0}],
		$$[{k},{k,p},{p}] ->   $$[{k},{0, q},{0}] + k.p $$[{},{0, q},{0}],
		$$[{p},{0,k},{k}] ->   $$[{p},{0, k},{0}] + p.k $$[{},{0, k},{0}],
		$$[{p},{0,k},{p}] ->   $$[{p},{p, q},{0}] + p.p $$[{},{p, q},{0}],
		$$[{p},{0,p},{k}] ->   $$[{p},{k,-q},{0}] + p.k $$[{},{k,-q},{0}],
		$$[{p},{0,p},{p}] ->   $$[{p},{0, p},{0}] + p.p $$[{},{0, p},{0}],
		$$[{p},{k,p},{k}] ->   $$[{p},{0, q},{0}] - p.k $$[{},{0, q},{0}],
		$$[{p},{k,p},{p}] ->   $$[{p},{0, q},{0}] + p.p $$[{},{0, q},{0}]
		
		(*
		$$[{x_,y_},{k,p},{k}] :>
			$$[{x,y},{0,q},{0}] - k.y $$[{x},{0,q},{0}]
			 - k.x $$[{y},{0,q},{0}] + k.x k.y $$[{},{0,q},{0}]
		,
		$$[{x_,y_},{k,p},{p}] :>
			- $$[{x,y},{0,q},{0}] - p.y $$[{x},{0,q},{0}]
			- p.x $$[{y},{0,q},{0}] - p.x p.y $$[{},{0,q},{0}]
		,
		
		$$[{x_,y_,z_},{0,k,p},{k}] :>
			$$[{x,y,z},{0,-k,q},{0}]
			- k.x $$[{y,z},{0,-k,q},{0}]
			- k.y $$[{x,z},{0,-k,q},{0}]
			- k.z $$[{x,y},{0,-k,q},{0}]
			+ k.x k.y $$[{z},{0,-k,q},{0}]
			+ k.x k.z $$[{y},{0,-k,q},{0}]
			+ k.y k.z $$[{x},{0,-k,q},{0}]
			- k.x k.y k.z $$[{},{0,-k,q},{0}]
		,
		$$[{x_,y_,z_},{0,k,p},{p}] :>
			$$[{x,y,z},{0,p,q},{0}]
			+ p.x $$[{y,z},{0,p,q},{0}]
			+ p.y $$[{x,z},{0,p,q},{0}]
			+ p.z $$[{x,y},{0,p,q},{0}]
			+ p.x p.y $$[{z},{0,p,q},{0}]
			+ p.x p.z $$[{y},{0,p,q},{0}]
			+ p.y p.z $$[{x},{0,p,q},{0}]
			+ p.x p.y p.z $$[{},{0,p,q},{0}]
		,
		
		$$[{},{0,k,p},{k}] -> - $$[{},{0,k,-q},{0}],
		$$[{},{0,k,p},{p}] -> - $$[{},{0,p, q},{0}]
		*)
	};
	
	result = Expand[expr /. simplifyRules
		/. {p.p -> 0, q.q -> 0, k.n -> x, n.p -> 1, n.q -> 1-x, eps^2 -> 0}
	];
	
	DEBUG[
		"$$SimplifyTranslate"
		,
		ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];
	
	result
];


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
		$$[{},{0,q},{ }] ->   Q (q.q)^(-eir) T0,
		$$[{},{k,p},{ }] ->   Q (q.q)^(-eir) T0,

		$$[{x_},{0,k},{}] :> - Q (k.k)^(-eir) k.x T1,
		$$[{x_},{0,p},{}] :> - Q (p.p)^(-eir) p.x T1,
		$$[{x_},{0,q},{}] -> - Q (q.q)^(-eir) q.x T1,
		$$[{x_},{k,p},{}] :> - Q (q.q)^(-eir) (q.x T1 - k.x T0),

		$$[{x_,y_},{0,z_},{}] :> Q (z.z)^(-eir) (x.z y.z T2 + x.y z.z T3),

		$$[{},{0,k,p},{}] -> Q (k.k)^(-1-eir) R0,
		$$[{x_},{0,k,p},{}] :> - Q (k.k)^(-1-eir) (p.x R1 + k.x R2),
		$$[{x_, y_},{0,k,p},{}] :> Q (k.k)^(-1-eir) (
    		p.x p.y R3 + k.x k.y R4 + (k.x p.y + p.x k.y) R5 + k.k x.y R6
   		),
   		$$[{x_,y_,z_},{0,k,p},{}] :> Q (k.k)^(-1-eir) (
   			  p.x p.y p.z H1
   			+ (p.x p.y k.z + p.x k.y p.z + k.x p.y p.z) H2
   			+ (p.x k.y k.z + k.x p.y k.z + k.x k.y p.z) H3
   			+ k.x k.y k.z H4
   			+ k.k (p.x y.z + p.y x.z + p.z x.y) H5
   			+ k.k (k.x y.z + k.y x.z + k.z x.y) H6
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
			p.x V1 + k.x V2 + n.x q.q/2 V3
		),
		
		$$[{x_},{p,q},{0}] :> Q (k.k)^(-eir)/p.n (
			p.x E1 + k.x E2 + n.x k.k/2 E3
		),
		$$[{x_},{k,-q},{0}] :> Q (p.p)^(-eir)/p.n (
			p.x F1 + k.x F2 + n.x p.p/2 F3
		),
		
		$$[{p,p}, {0,p}, {k}] -> Q (p.p)^(-eir) X0
		,
		$$[{p,p}, {k,p}, {k}] -> Q (q.q)^(-eir) Y0
		,
		
		$$[{},{0,k, p},{0}] -> - Q (k.k)^(-1-eir) / p.n S0,
		
		$$[{x_},{0,k,p},{0}] :> Q (k.k)^(-1-eir) / p.n (
			p.x S1 + k.x S2 + n.x k.k/(2 k.n) S3
		),
		
		$$[{x_, y_},{0,k,p},{0}] :> Q (k.k)^(-1-eir)/p.n (
			  S4 p.x p.y
			+ S5 (p.x k.y + k.x p.y)
			+ S6 k.x k.y
			+ S7 (p.x n.y + n.x p.y) k.k/(2 p.n)
			+ S8 (k.x n.y + n.x k.y) k.k/(2 k.n)
			+ S9 n.x n.y (k.k/(2 k.n))^2
			+ S10 x.y k.k
		),
		
		
		$$[{},{0,k,p},{k}] -> - Q (k.k)^(-1-eir)/q.n U0,
		
		$$[{x_},{0,k,p},{k}] :> Q (k.k)^(-1-eir)/q.n (
			p.x U1 + k.x U2 + n.x k.k/(2 k.n) U3
		),
				
		$$[{x_, y_},{0,k,p},{k}] :> Q (k.k)^(-1-eir)/q.n (
			  U4 p.x p.y
			+ U5 (p.x k.y + k.x p.y)
			+ U6 k.x k.y
			+ U7 (p.x n.y + n.x p.y) k.k/(2 p.n)
			+ U8 (k.x n.y + n.x k.y) k.k/(2 k.n)
			+ U9 n.x n.y (k.k/(2 k.n))^2
			+ U10 x.y k.k
		),
		
		
		$$[{},{0,k,p},{p}] -> Q (k.k)^(-1-eir) k.n/q.n W0,
		
				
		$$[{x_},{0,k,p},{p}] :> Q (k.k)^(-1-eir)/q.n (
			p.x W1 + k.x W2 + n.x k.k/(2 k.n) W3
		),
		
		$$[{x_, y_},{0,k,p},{p}] :> Q (k.k)^(-1-eir)/q.n (
			  W4 p.x p.y
			+ W5 (p.x k.y + k.x p.y)
			+ W6 k.x k.y
			+ W7 (p.x n.y + n.x p.y) k.k/(2 p.n)
			+ W8 (k.x n.y + n.x k.y) k.k/(2 k.n)
			+ W9 n.x n.y (k.k/(2 k.n))^2
			+ W10 x.y k.k
		)
	};
	
	psRule = {
		Q -> I (4 Pi)^(-2+eir) Gamma[1+eir]
	};

	result = Expand[expr /. integrateRules /. psRule];

	(*
	Print[Expand[result]];
	*)
	
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


$$ExpandCommon[expr_] := Module[
	{},
	
	Expand[expr //. {
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

		T0 -> 1/euv + 2,
		T1 -> 1/(2 euv) + 1,
		T2 -> 1/(3 euv) + 13/18,
		T3 -> -1/(12 euv) - 2/9,

		V1 ->   (1-x + x Log[x])/(1-x)^2 / euv,
		V2 -> - (1-x + Log[x])/(1-x)^2 / euv
	}]
];


$$ExpandMPV[expr_] := Module[
	{},
	
	Expand[$$ExpandCommon[expr] //. {
		H1 -> (-11/3 + 2 I0 + Log[1-x])/eir - 85/9,
		H2 -> 1/(3 eir) + 11/9, 
		H3 -> 1/(6 eir) + 4/9,
		H4 -> 1/(3 eir) + 13/18,
		H5 -> - 1/(12 euv) - 11/36,
		H6 -> - 1/(12 euv) - 2/9,
		
		R0 -> - ((2 I0 + Log[1-x])/eir - 4 I1 + 2 I0 Log[1-x] + (Log[1-x]^2)/2),
		R1 ->   R0 + 2/eir + 4,
		R2 -> - 1/eir - 2,
		R3 ->   R0 + 3/eir + 7,
		R4 -> - 1/(2 eir) - 1,
		R5 -> - 1/(2 eir) - 3/2,
		R6 ->   1/(4 euv) + 3/4,
		
		S0 -> - ((3 I0 + Log[1-x] - Log[x]) / eir - 5 I1 + 2 I0 Log[1-x] + I0 Log[x] + (Log[x]^2)/2 + (Log[1-x]^2)/2 + 2 Li2[1-x] + Li2[1]),
		
		S1 -> (-2 x Li2[1 - x] + (-1 + x) (8 I1 - 4 I0 Log[1 - x] - Log[1 - x]^2))/(2 (-1 + x)) + (-2 I0 (-1 + x) - (-1 + x) Log[1 - x] + x Log[x])/(eir (-1 + x)),
		S2 -> Li2[1 - x]/(-1 + x) - Log[x]/(eir (-1 + x)),
		S3 -> (I0 - I0 x + Log[x])/(euv (-1 + x)) + (-2 x Li2[1 - x] + (-1 + x) (2 I1 - 2 Li2[1] - 2 I0 Log[x] - Log[x]^2))/(2 (-1 + x)),
  		
		S4 -> ((2 + 2 I0 (-1 + x) - x) (-1 + x) + (-1 + x)^2 Log[1 - x] - x^2 Log[x])/(eir (-1 + x)^2) + (1/(2 (-1 + x)^2))(2 x^2 Li2[1 - x] + 4 I0 (-1 + x)^2 Log[1 - x] + (-1 + x)^2 Log[1 - x]^2 - 2 (2 (-1 + x) (-2 + 2 I1 (-1 + x) + x) + x^2 Log[x])),
		S5 -> (1 - x + x Log[x])/(eir (-1 + x)^2) + (2 - 2 x - x Li2[1 - x] + x Log[x])/(-1 + x)^2,
		S6 -> (-1 + x - Log[x])/(eir (-1 + x)^2) + (-2 + 2 x + Li2[1 - x] - Log[x])/(-1 + x)^2,
		S7 -> (-1 + x - Log[x])/(euv (-1 + x)^2) + (2 (-1 + x) + Li2[1 - x] + (-2 + x) Log[x])/(-1 + x)^2,
		S8 -> (1 - x + x Log[x])/(euv (-1 + x)^2) + (2 - 2 x - x Li2[1 - x] + x Log[x])/(-1 + x)^2,
		S9 -> ((2 + I0 (-1 + x) - x) (-1 + x) + (1 - 2 x) Log[x])/(euv (-1 + x)^2) + (1/(2 (-1 + x)^2))(-2 (-1 + x) (-4 + I1 (-1 + x) + 2 x + Li2[1] - x Li2[1]) + 2 x^2 Li2[1 - x] + 2 (I0 (-1 + x)^2 - x^2) Log[x] + (-1 + x)^2 Log[x]^2),
		S10 -> (Li2[1 - x] - 2 Log[x])/(2 (-1 + x)) + Log[x]/(euv (2 - 2 x)),
		
		U0 -> - ((3 I0 + 3 Log[1-x] - Log[x])/eir - 5 I1 + 2 I0 Log[1-x] + I0 Log[x] + (Log[x]^2)/2 - (Log[1-x]^2)/2 - 2 Li2[1-x] + 5 Li2[1]),
		
		U1 -> 1/2 (8 I1 - 4 x Li2[1] + 2 x Li2[1 - x] - 4 I0 Log[1 - x] + (-1 + x) Log[1 - x]^2) + (-2 I0 - (1 + x) Log[1 - x] + x Log[x])/eir,
		U2 -> I1 - 3 Li2[1] + Li2[1 - x] + (-I0 - Log[1 - x])/eir + 1/2 Log[1 - x]^2 - I0 Log[x] - Log[x]^2/2,
		U3 -> -I1 + Li2[1] + 2 x Li2[1] - x Li2[1 - x] - 1/2 x Log[1 - x]^2 + I0 Log[x] + Log[x]^2/2 + (I0 + x Log[1 - x] + Log[x] - x Log[x])/euv,
		
		U4 -> 1/2 (-8 - 8 I1 + 4 x + 4 x^2 Li2[1] - 2 x^2 Li2[1 - x] + 2 (2 I0 + x^2) Log[1 - x] - (-1 + x^2) Log[1 - x]^2 - 2 x^2 Log[x]) + (-2 + 2 I0 + x + (1 + x^2) Log[1 - x] - x^2 Log[x])/eir,
		U5 -> 2 + 1/eir - x Log[1 - x] + x Log[x],
		U6 -> -2 - I1 + 3 Li2[1] - Li2[1 - x] + Log[1 - x] - 1/2 Log[1 - x]^2 + (-1 + I0 + Log[1 - x])/eir - Log[x] + I0 Log[x] + Log[x]^2/2,
		U7 -> (-1 + x) Li2[1 - x] + 1/2 (-4 + 4 Li2[1] - 4 x Li2[1] - 2 (-2 + x) Log[1 - x] + (-1 + x) Log[1 - x]^2 + 2 (-2 + x) Log[x]) + (-1 - (-1 + x) Log[1 - x] + (-1 + x) Log[x])/euv,
		U8 -> 2 + I1 - Li2[1] - 2 x Li2[1] + x Li2[1 - x] - x Log[1 - x] + 1/2 x Log[1 - x]^2 - I0 Log[x] + x Log[x] - Log[x]^2/2 + (1 - I0 - x Log[1 - x] + (-1 + x) Log[x])/euv,
		U9 -> -4 - I1 + 2 x + Li2[1] + 2 x^2 Li2[1] - x^2 Li2[1 - x] + x^2 Log[1 - x] - 1/2 x^2 Log[1 - x]^2 + I0 Log[x] - x^2 Log[x] + Log[x]^2/2 + (-2 + I0 + x + x^2 Log[1 - x] + Log[x] - x^2 Log[x])/euv,
		U10 -> 1/4 (-1 + x) (4 Li2[1] - 2 Li2[1 - x] + 4 Log[1 - x] - Log[1 - x]^2 - 4 Log[x]) + ((-1 + x) (Log[1 - x] - Log[x]))/(2 euv),
		
		W0 -> - ((3 I0 + 3 Log[1-x] - 2 Log[x])/eir - 5 I1 + 2 I0 Log[1-x] - 3 I0 Log[x] - (Log[1-x]^2)/2 + Log[x] Log[1-x] + 2 Li2[1-x] + Log[x]^2 + 5 Li2[1]),
		W1 -> 4 I1 - 7 I1 x + 5 x Li2[1] + 2 x Li2[1 - x] - 1/4 (2 + x) Log[1 - x]^2 - 3 I0 x Log[x] + x Log[x]^2 + (-2 I0 + 4 I0 x + (-1 + 3 x) Log[1 - x] - 2 x Log[x])/eir + Log[1 - x] (-2 I0 + 3 I0 x + x Log[x]),
		W2 -> -2 I1 + I0 Log[1 - x] + 1/4 Log[1 - x]^2 + (I0 + Log[1 - x])/eir,
		W3 -> 2 I1 x + (-I0 x - Log[1 - x])/euv - I0 x Log[1 - x] - 1/4 x Log[1 - x]^2,
		
		W4 -> (-2 + 4 I0 + x - 5 I0 x + (2 - 3 x) Log[1 - x] + 2 x Log[x])/eir - (1/(2 (-1 + x)))(-8 - 16 I1 + 12 x + 34 I1 x + 4 x^2 - 18 I1 x^2 - 4 x^3 - 10 x Li2[1] + 10 x^2 Li2[1] + 4 (-1 + x) x Li2[1 - x] + 4 (-1 + x) x Li2[x] + 8 I0 Log[1 - x] + 2 x Log[1 - x] - 16 I0 x Log[1 - x] - 2 x^2 Log[1 - x] + 8 I0 x^2 Log[1 - x] + 2 Log[1 - x]^2 - 3 x Log[1 - x]^2 + x^2 Log[1 - x]^2 + 6 I0 x Log[x] - 6 I0 x^2 Log[x] - 2 x Log[1 - x] Log[x] + 2 x^2 Log[1 - x] Log[x] - 2 x Log[x]^2 + 2 x^2 Log[x]^2),
		W5 -> (1 - I0 - Log[1 - x])/eir + (8 (-1 + I1 (-1 + x) + 3 x - x^2) + 8 (-1 + x) Li2[x] - 4 (1 + I0) (-1 + x) Log[1 - x] + (-1 + x) Log[1 - x]^2)/(4 (-1 + x)),
		W6 -> -(1/eir) - (4 x + 4 (-1 + x) Li2[x] - 2 (-1 + x) Log[1 - x] + (-1 + x) Log[1 - x]^2)/(2 (-1 + x) x),
		W7 -> (-1 + I0 + Log[1 - x]/x)/euv + (1/(4 (-1 + x) x))(-8 x (3 + I1 (-1 + x) - 3 x + x^2) + 8 (2 - 3 x + x^2) Li2[x] + 4 (-1 + x) (2 + (-1 + I0) x) Log[1 - x] + (4 - 7 x + 3 x^2) Log[1 - x]^2),
		W8 -> (x - (-1 + x) Log[1 - x])/(euv x) + (4 x + 4 (-1 + x) Li2[x] - 2 (-1 + x) x Log[1 - x] + (-1 + x) Log[1 - x]^2)/(2 (-1 + x) x),
		W9 -> ((-2 + x) x + 2 (-1 + x) Log[1 - x])/(euv x) + (1/(2 (-1 + x) x))(4 x (-2 + 3 x - 3 x^2 + x^3) - 4 (-2 + 4 x - 3 x^2 + x^3) Li2[x] + 2 (-1 + x) x^2 Log[1 - x] - (-2 + 4 x - 3 x^2 + x^3) Log[1 - x]^2),
		W10 -> ((-1 + x) Log[1 - x])/(2 euv x) + (4 (-2 + x) x - 4 (-1 + x) Li2[x] + 4 (-1 + x) Log[1 - x] - (-1 + x) Log[1 - x]^2)/(4 x)
	}]
]


$$ExpandPV[expr_] := Module[
	{},
	
	Expand[$$ExpandCommon[expr] //. {
		R0 ->  1/eir^2 - Li2[1],
		R1 ->  1/eir^2 + 2/eir + 4 - Li2[1],
		R2 -> -1/eir - 2,
		R3 ->  1/eir^2 + 3/eir + 7 - Li2[1],
		R4 -> -1/(2 eir) - 1,
		R5 -> -1/(2 eir) - 3/2,
		R6 ->  1/(4 euv) + 3/4,
		
		U0 -> 1/eir^2 + (-I0 + Log[x] - 2 Log[1-x])/eir + I1 -I0 Log[x] + 2 Li2[1-x] - (Log[x]^2)/2 + Log[1-x]^2 - 6 Li2[1],

		S0 -> 1/eir^2 - (I0 - Log[x])/eir + I1 - I0 Log[x] - 2 Li2[1]
				- 2 Li2[1-x] - (Log[x]^2)/2,
		
		S1 -> 1/eir^2 - x Log[x]/((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
		S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x)
	}]
]


Options[IntegrateLoop] = {Prescription -> "MPV"};
IntegrateLoop[expr_, l_, OptionsPattern[]] := Module[
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

	integratedPV =  If[
		OptionValue[Prescription] == "MPV"
		,
		$$ExpandMPV[integrated]
		,
		If[OptionValue[Prescription] == "PV"
		,
		$$ExpandPV[integrated]
	]];

	{
		{"collected", collected},
		{"simplified", simplified},
		{"integrated", {
			{"short", integrated},
			{"long", integratedPV}}}
	}
];

End[]


PaVeReduce[lhs_, rhs_, basis_, vars_] := Module[
	{result},

	$PutOnShell[expr_] := Replace[
		Expand[expr] /. {
			S[x_,x_]^(-eir) :> 0 /; (x == p || x == q)
			,
			S[x_,x_]^(n_Integer-eir) :> 0 /; n > 0 && (x == p || x == q)
			,
			S[x_,x_]^n_Integer :> 0 /; n > 1 && (x == p || x == q)
		}
		,
		{
			p.p -> 0,
			q.q -> 0,
			n.n -> 0,
			(n.n)^2 -> 0,
			k.p -> (k.k)/2, (k.p)^n_ :> ((k.k)/2)^n}
		,
		2
	];


	$equations = {};
	For[i = 1, i <= Length[basis], i++,
		ff = basis[[i]];	
		$lhs = IntegrateLoop[lhs  ff, l];
		$lhs = $Get[$lhs, {"integrated", "short"}];
		$lhs = $PutOnShell[$lhs];
		$lhs = $lhs /. {n.p -> 1, k.n -> x, n.q -> 1-x};
		(*
		$lhs = $lhs /. {eir -> 0};
		*)
		
		$rhs = Simplify[rhs ff];
		$rhs = $PutOnShell[$rhs];
		$rhs = $rhs /. {Global`d -> 4 - 2 eps, n.p -> 1, k.n -> x, n.q -> 1-x};
		
		Print[$lhs == $rhs];
		AppendTo[$equations, $lhs == $rhs];
	];
	
	result = First[Solve[$equations, vars]];
	result = Simplify[$$ExpandMPV[result] /. {eir -> eps, euv -> eps}];
	result = result /. {Rule[e1_, e2_] :> Rule[e1, Normal[Simplify[Series[e2, {eps, 0, 0}]]]]};
	result
	(*
	result = $$SimplifyAlgebraic[integral];
	
	$PutOnShell[result] /. {n.p -> 1, k.n -> x, n.q -> 1-x}
	*)
]


EndPackage[]
