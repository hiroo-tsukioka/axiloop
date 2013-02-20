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

(*============================================================================*)
(*                                                                            *)
(*  Axiloop -- a package for Wolfram Mathematica that can calculate DGLAP     *)
(*             evolution kernels up to next-to-leading order.                 *)
(*                                                                            *)
(*  Author:   Oleksandr Gituliar <oleksandr@gituliar.org>                     *)
(*  Created:  04-05-2012                                                      *)
(*  Homepage: http://gituliar.org/axiloop.html                                *)
(*                                                                            *)
(*============================================================================*)


BeginPackage["Axiloop`", {"Axiloop`Tracer`"}]

Clear[ "Axiloop`*" , "Axiloop`Private`*"];


(*---------------------------------------------------------------------------*)
(*---------------------- FEYNMAN RULES and GAMMA TRACE ----------------------*)
(*---------------------------------------------------------------------------*)

FP::usage = "FP[momentum, Line -> f1] -- a fermion propagator in the light-cone
gauge."

FPx::usage = "FPx[momentum, Line -> f1] -- a crossed (final-state, on-shell)
fermion propagator in the light-cone gauge."

FV::usage = "FV[index, Line -> f1] -- a fermion vertex in the light-cone gauge."

G::usage = "G[<vector or index>, Line -> f1] -- a gamma matrix.

Usage:
    G[{mu}]     a gamma matrix with vector index `mu`;
    G[p]        a gamma matrix convoluted with a vector,
                the same as `G[{mu}] p.{mu}`;"

GP::usage = "GP[mu, nu, p] -- a gluon propagator in the light-cone gauge."

GPx::usage = "GPx[mu, nu, p] -- a crossed (final-state, on-shell) gluon
propagator in the light-cone gauge."

GV::usage = "GV[i1,p1, i2,p2, i3,p3] -- a gluon vertex in the light-cone gauge."


(*---------------------------------------------------------------------------*)

eps::usage =
	"Dimensional regulator; n = 4 + 2 eps."

eir::usage =
	"IR pole in 4 - 2 eta dimensions."

euv::usage =
	"UV pole in 4 - 2 eta dimensions."

g::usage =
	"Quark-gluon coupling constant."

k::usage =
	"Outgoing particle momentum; k.n = x."

l::usage =
	"Loop momentum."

n::usage =
	"Light-cone gauge vector; n.n = 0."

p::usage =
	"Incoming particle momentum; p.n = 1."

q::usage = "Final state particle momentum; q.q = 0."

x::usage =
	"x = k.n/p.n"

PolePart::usage =
	"PolePart[expr, x] extract coefficient in front of 1/x in expr."

GammaTrace::usage =
"GammaTrace[expr, NumberOfDimensions -> 4 + 2 eps] calculates trace
of the gamma matrices product in arbitrary number of dimensions. Be
sure to use non-commutative product `**` operation instead of commonly
used commutative product `*`.

Example:

   	In[1] := GammaTrace[G[{mu}]**G[{mu}]]
   	Out[1] = 4 (4 + 2 eps)
   	
   	In[2] := GammaTrace[G[{mu}]**G[{mu}], NumberOfDimensions -> ndim]
   	Out[2] = 4 ndim"

$Get::usage =
	"$Get[hash_, key_] get value by key from a hash table."

I0::usage =
	"Principal Value regulated integral; I0 = - Log[delta] + O[delta]."

I1::usage =
	"Principal Value regulated integral; I1 = - (Log[delta]^2)/2 - Li2[1]/4 + O[delta]."

IntegrateFinal::usage =
	"Integrate over final-state momenta."

IntegrateLoop::usage =
	"Integrate over loop momenta."

Li2::usage =
	"Dilogarythm function; Li2[x] = - Integrate[Log[1-t]/t, {t, 0, x}]."

PartonDensity::usage =
	"Kernel constructor; define and integrate a kernel."

SplittingFunction::usage = ""


Begin["`Private`"]


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)

$debug = True;

DebugInfo[sender_, message_] := If[
	$debug
	,
	Print[
		"DebugInfo::"
		,
		sender
		,
		" : "
		,
		message
	]
];


DebugInfo[
	"Axiloop"
	,
	"Entering AXILOOP"
];


$Get[hash_, keys_, default_:Null] := Module[
	{item, key, value},
	
	key = If[
		ListQ[keys]
		,
		First[keys]
		,
		keys
	];

	item = Select[hash, First[#] == key &, 1];

	value = If[
		item == {}
		,
		default
		,
		Last[First[item]]
	];
	
	If[
		!ListQ[keys] || Length[keys] == 1
		,
		value
		,
		$Get[value, Rest[keys]]
	]
];


(* Useful modifications to standard functions *)

Unprotect[Dot];
    (-x_).y_ := -x.y;
Protect[Dot];


CollectExclusiveShort[expr_] := Module[{},
	Collect[
		expr
			/. {eps->0, (k.k)^(n_Integer-eir):>(k.k)^n, p.p->0, q.q->0}
			/. {0^(-eir)->1, 0^(1-eir)->0, 0^(2-eir)->0}
		,
		{B0,B1,B3,C0,C1,D0,K0,P0,P1,P3,R0,R1,R2,R3,R4,R5,R6,S0,S1,S2,T0,T1,V1,V2,U0}
		,
		Simplify
	]
];


(*---------------------------------------------------------------------------*)
(*---------------------- FEYNMAN RULES and GAMMA TRACE ----------------------*)
(*---------------------------------------------------------------------------*)

$fermionLines = {};   (* A list of fermion lines used by user. `GammaTrace`  *)
                      (* calculates trace over each line from that list.     *)
                      (* By default `f1` line is used.                       *)

Options[G] = {Line -> f1};
G[x_, OptionsPattern[]] := (
	$fermionLines = Union[$fermionLines, {OptionValue[Line]}];
	GTrace[OptionValue[Line], x]
);

Options[FP] = {Line -> f1};
FP[p_, OptionsPattern[]] := 1/p.p FPx[p, Line -> OptionValue[Line]];

Options[FPx] = {Line -> f1};
FPx[p_, OptionsPattern[]] := I G[p, Line -> OptionValue[Line]];

Options[FV] = {Line -> f1};
FV[mu_, OptionsPattern[]] := - I g G[{mu}, Line -> OptionValue[Line]];

GP[mu_, nu_, p_] := 1/p.p GPx[mu, nu, p];

GPx[mu_, nu_, p_] := - I ({mu}.{nu} - (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n)

GV[i1_,p1_, i2_,p2_, i3_,p3_] :=
	g ( {i1}.{i2} (p1.{i3}-p2.{i3})
	  + {i2}.{i3} (p2.{i1}-p3.{i1})
	  + {i3}.{i1} (p3.{i2}-p1.{i2})
);

Options[GammaTrace] = {NumberOfDimensions -> 4 + 2 eps};
GammaTrace[expr_, OptionsPattern[]] := Module[
	{$ndim = OptionValue[NumberOfDimensions], $result},
	
	Spur[f0];
	$result = Block[
		{Global`d = $ndim},
		
		Expand[expr
			/. ((#->f0)& /@ $fermionLines)
			/. NonCommutativeMultiply -> Times
		]
	];
	NoSpur[f0];
	$result
];


(*---------------------------------------------------------------------------*)
(*--------------------- FINAL-STATE MOMENTA INTEGRATION ---------------------*)
(*---------------------------------------------------------------------------*)

IntegrateFinal[kernel_, ndim_] := Module[
	{eps},

	eps = Simplify[ndim/2 - 2];
	(4 Pi)^(-2+eps)/Gamma[1-eps] (1 + eps Log[1-x]) Integrate[(k.k)^(eps) kernel, k.k]
];


(*---------------------------------------------------------------------------*)
(*------------------------ LOOP MOMENTA INTEGRATION -------------------------*)
(*---------------------------------------------------------------------------*)


ExpandLoopIntegrals[expr_, l_] := Module[
	{expandRules},
	
	expandRules = {
		$$[{x_,a___},{b___},{c___}] :> $$[{a},{b},{c}] l.x,
		$$[{a___},{x_,b___},{c___}] :> $$[{a},{b},{c}] / (l+x).(l+x),
		$$[{a___},{b___},{x_,c___}] :> $$[{a},{b},{c}] / (l+x).n,
		$$[{},{},{}] -> 1
	};
	
	Expand[expr //. expandRules]
];


CollectLoopIntegrals[expr_, l_] := Module[
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
		/. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]];

	DebugInfo[
		"CollectLoopIntegrals"
		,
		StringDrop[ToString[#], 16] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];
	(*
	DebugInfo[
		"CollectLoopIntegrals::result"
		,
		result
	];
	*)
	result
];


SimplifyLoopIntegrals[expr_] := Module[
	{result, signCorrectionRules, simplifyRules},
		
	signCorrectionRules = {
		$$[{a___}, {b___}, {c___}] :>
			(-1)^(Length[Select[{a}, !MatchQ[#,l] &]]+Length[{c}]) $$[{a}, -# &/@ {b}, -# &/@ {c}] /;
				{b} == Select[{b}, Or[# == 0, MatchQ[#, Times[-1, _Symbol]]] &]
	};

	simplifyRules = {
		(*
		$$[{a1___,x_,a2___},{0,b1___,x_,b2___},{c___}] :> 1/2 (
			$$[{a1,a2}, {0,b1,b2}, {c}] - $$[{a1,a2}, {b1,x,b2}, {c}] - $$[{a1,a2}, {0,b1,x,b2}, {c}] x.x
		)
		*)
		
		
		$$[{x_,y_}, {0,q}, {}] :>
			$$[{x,y}, {k,p}, {}] + k.x k.y $$[{}, {k,p}, {}]
			+ k.x $$[{y}, {k,p}, {}] + k.y $$[{x}, {k,p}, {}] 
		,
		(*
		$$[{k,k}, {0,k,p}, {0}] -> 1/2 (
			$$[{k}, {0,p}, {0}] - $$[{k}, {k,p}, {0}] - k.k $$[{k}, {0,k,p}, {0}]
		)
		,
		$$[{k,p}, {0,k,p}, {0}] -> 1/2 (
			$$[{p}, {0,p}, {0}] - $$[{p}, {k,p}, {0}] - k.k $$[{p}, {0,k,p}, {0}]
		)
		,
		$$[{p,p}, {0,k,p}, {0}] -> 1/2 (
			$$[{p}, {0,k}, {0}] - $$[{p}, {k,p}, {0}] - p.p $$[{p}, {0,k,p}, {0}]
		)
		,
		*)
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
		(*
		$$[{a1___,n,a2___}, {b___}, {x_,c___}] :>
			$$[{a1,a2}, {b}, {c}] - x.n $$[{a1,a2}, {b}, {x,c}]
		,
		*)
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
		$$[x__, {k,p}] :> ($$[x, {p}] - $$[x, {k}]) / (k.n-p.n)
		,
		
		(* l.l terms in the numerator *)
		$$[{l}, {k,p}, {c_}] :>
			$$[{}, {p}, {c}] - 2 $$[{k}, {k,p}, {c}] - k.k $$[{}, {k,p}, {c}]
	};
		
	result = expr
		/. signCorrectionRules
		//. simplifyRules;
	
	DebugInfo[
		"SimplifyLoopIntegrals"
		,
		StringDrop[ToString[#], 16] &/@ Union[Cases[{result}, $$[__], Infinity]]
	];
	
	result
];


$kinematicRules = {
	k.p -> (p.p + k.k - q.q) / 2,
	k.q -> (p.p - k.k - q.q) / 2,
	p.q -> (p.p - k.k + q.q) / 2,

	n.n -> 0
};

$onShellRules = {
	p.p -> 0,
	q.q -> 0
}

IntegrateLoop::unevaluated = "Unknown integral(s): `1`."

IntegrateLoop[expr_, l_] := Module[
	{collected, expansionRules, integratedLong, integratedShort,
	 integrationRules, phaseSpaceRule, simplified, simplifyRules,
	 translationRules, unevaluated},
	
	translationRules = {
		$$[{}, {q}, {}] -> $$[{}, {0}, {}],
		
		$$[{}, {p}, {p}] -> $$[{}, {0}, {0}]
		,
		$$[{}, {p}, {k}] -> $$[{}, {q}, {0}]
		,
		
		$$[{ },{0,k},{k}] -> - $$[{ },{0, k},{0}],
		$$[{ },{0,k},{p}] -> - $$[{ },{p, q},{0}],
		$$[{ },{0,p},{k}] -> - $$[{ },{k,-q},{0}],
		$$[{ },{0,p},{p}] -> - $$[{ },{0, p},{0}],
		$$[{ },{k,p},{k}] ->   $$[{ },{0, q},{0}],
		$$[{ },{k,p},{p}] -> - $$[{ },{0, q},{0}],
		
		$$[{k},{0,k},{k}] ->   $$[{k},{0, k},{0}] + k.k $$[{},{0,k},{0}],
		$$[{k},{0,k},{p}] ->   $$[{k},{0, q},{0}] + k.p $$[{},{0,q},{0}],
		$$[{k},{0,p},{p}] ->   $$[{k},{0, p},{0}] + k.p $$[{},{0,p},{0}],
		$$[{k},{k,p},{k}] ->   $$[{k},{0, q},{0}] - k.k $$[{},{0,q},{0}],
		$$[{k},{k,p},{p}] ->   $$[{k},{0, q},{0}] + k.p $$[{},{0,q},{0}],
		$$[{p},{0,k},{k}] ->   $$[{p},{0, k},{0}] + k.p $$[{},{0,k},{0}],
		$$[{p},{k,p},{k}] ->   $$[{p},{0, q},{0}] - k.p $$[{},{0,q},{0}],
		$$[{p},{k,p},{p}] ->   $$[{p},{0, q},{0}] + p.p $$[{},{0,q},{0}],
		
		$$[{},{0,k,p},{k}] -> - $$[{},{0,k,-q},{0}],
		$$[{},{0,k,p},{p}] -> - $$[{},{0,p, q},{0}]
	};
	
	integrationRules = {
		$$[{},{0},{ }] -> 0,
		$$[{},{0},{0}] -> 0,
		
		$$[{},{0,k},{ }] ->   Q (k.k)^(-eir) T0,
		$$[{},{0,p},{ }] ->   Q (p.p)^(-eir) T0,
		$$[{},{k,p},{ }] ->   Q (q.q)^(-eir) T0,

		$$[{x_},{0,k},{}] :> - Q (k.k)^(-eir) k.x T1,
		$$[{x_},{0,p},{}] :> - Q (p.p)^(-eir) p.x T1,
		$$[{x_},{0,q},{}] -> - Q (q.q)^(-eir) q.x T1,
		$$[{x_},{k,p},{}] :> - Q (q.q)^(-eir) (q.x T1 - k.x T0),
		(*
		$$[{x_,y_},{k,p},{}] :> Q (q.q)^(-eir) (
			x.y W0 + k.x k.y W1 + (k.x p.y + k.y p.x)W2 + p.x p.y W3
		),
		*)
		$$[{},{0,k,p},{}] -> Q (k.k)^(-1-eir) R0,
		$$[{x_},{0,k,p},{}] :> - Q (k.k)^(-1-eir) (p.x R1 + k.x R2),
		$$[{x_, y_},{0,k,p},{}] :> Q (k.k)^(-1-eir) (
    		p.x p.y R3 + k.x k.y R4 + (k.x p.y + p.x k.y) R5 + k.k x.y R6
   		),
   

		$$[{},{0, k},{0}] -> - Q (k.k)^(-eir) P0 / k.n,
		$$[{},{0, p},{0}] -> - Q (p.p)^(-eir) B0 / n.p,
		$$[{},{0, q},{0}] -> - Q (q.q)^(-eir) C0 / n.q,
		$$[{},{k, p},{0}] -> - Q (q.q)^(-eir) K0 / n.p,
		$$[{},{k,-q},{0}] -> - Q (p.p)^(-eir) D0 / n.p,
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
		
		$$[{},{0,k, p},{0}] -> - Q (k.k)^(-1-eir)/n.p S0,
		$$[{},{0,k,-q},{0}] -> - Q (k.k)^(-1-eir)/q.n U0,
		
		$$[{},{0,p, q},{0}] -> - Q (k.k)^(-1-eir)/k.n V0,
		
	
		$$[{x_},{0,k,p},{0}] :> - Q (k.k)^(-1-eir)/p.n (
			p.x S1 + k.x S2 + n.x k.k/(2 k.n) S3
		)
	};
	
	simplifyRules = {
		$$[{}, {k}, {0}] ->
			$$[{}, {0}, {0}] - 2 $$[{k}, {0,k}, {0}] - k.k $$[{}, {0,k}, {0}]
		,
		$$[{}, {q}, {0}] ->
			$$[{}, {0}, {0}] - 2 $$[{q}, {0,q}, {0}] - q.q $$[{}, {0,q}, {0}]
	};
	
	phaseSpaceRule = {
		Q -> I (4 Pi)^(-2+eir) Gamma[1+eir]
	};
	
	expansionRules = {
		B0 -> I0/euv - I1 + Li2[1],
		B1 -> 1/euv + 2,
		B3 -> (I0 - 2)/euv - I1 - 4 + Li2[1],

		C0 -> (I0 + Log[1-x])/euv - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1],
		C1 -> 1/euv + 2,
		C3 -> (I0 + Log[1-x] - 2)/euv - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 - 4
				+ Li2[1],
		
		D0 -> (Log[1-x] - Log[x])/euv + (Log[x]^2)/2 - (Log[1-x]^2)/2 + Li2[1]
				- 2 Li2[1-x] -Log[x]Log[1-x],
		
		K0 -> - Log[x]/((1-x) euv),

		P0 -> (I0 + Log[x])/euv - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
		P1 -> 1/euv + 2,
		P3 -> (I0 + Log[x] - 2)/euv - I1 + I0 Log[x] + (Log[x]^2)/2 - 4
				+ Li2[1],
	
		R0 ->  1/eir^2 - Li2[1],
		R1 ->  1/eir^2 + 2/eir + 4 - Li2[1],
		R2 -> -1/eir - 2,
		R3 ->  1/eir^2 + 3/eir + 7 - Li2[1],
		R4 -> -1/(2 eir) - 1,
		R5 -> -1/(2 eir) - 3/2,
		R6 ->  1/(4 euv) + 3/4,
		
		S0 -> 1/eir^2 - (I0 - Log[x])/eir + I1 - I0 Log[x] - 2 Li2[1]
				- 2 Li2[1-x] - (Log[x]^2)/2,
		S1 -> 1/eir^2 - x Log[x]/((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
		S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x),
		
		T0 -> 1/euv + 2,
		T1 -> 1/(2 euv) + 1,

		V1 -> (1-x + x Log[x])/(1-x)^2 / euv,
		V2 -> - (1-x + Log[x])/(1-x)^2 / euv
		(*
		W0 -> 0 1/euv,
		W1 -> 0 1/euv,
		W2 -> 0 1/euv,
		W3 -> 0 1/euv
		*)
	};
	
	collected = CollectLoopIntegrals[expr, l];
	
	simplified = SimplifyLoopIntegrals[collected];

	integratedShort = Expand[
		simplified
			/. translationRules
			/. simplifyRules
			/. integrationRules
			/. phaseSpaceRule
			/. $kinematicRules
	];
	(*
	DebugInfo["IntegratedShort", integratedShort];
	*)
	unevaluated = Union[Cases[integratedShort, $$[__], Infinity]];
	If[
		unevaluated != {}
		,
		Message[
			IntegrateLoop::unevaluated,
			StringDrop[ToString[#], 16] &/@ unevaluated
		];
		Return[Null]
	];
	
	integratedLong = Expand[
		integratedShort
			/. expansionRules
	];

	{
		{"collected", collected},
		{"simplified", simplified},
		{"integrated", {
			{"short", integratedShort},
			{"long", integratedLong}}}
	}
];


PolePart[kernel_, eta_] := Expand[
	Coefficient[Series[kernel, {eta, 0, 1}], eta, -1]
];


(*---------------------------------------------------------------------------*)
(*--------------------------- SPLITTING FUNCTION ----------------------------*)
(*---------------------------------------------------------------------------*)

SplittingFunction[$topology_, $LO_:Null] := Module[
	{counterterm, exclusive, exclusiveBare, inclusive, integrated, trace, Z,
	 $PutOnShell},
	
	$PutOnShell[expr_] := Replace[
		expr /. {
			S[x_,x_]^(n_Integer-eir) :> 0 /; n > 0 && (x == p || x == q)
			,
			S[x_,x_]^n_Integer :> 0 /; n > 1 && (x == p || x == q)
		}
		,
		{p.p -> 0, q.q -> 0}
		,
		2
	];
	
	trace = Expand[
		GammaTrace[$topology, NumberOfDimensions -> 4 + 2 eps]
			/. $kinematicRules
	];

	integrated = IntegrateLoop[trace, l]; 
	If[
		integrated == Null
		,
		Return[Null]
	];
	
	exclusiveBare = If[
		SameQ[$LO, Null]
		,
		Null
		,
		$PutOnShell[$Get[integrated, {"integrated", "short"}]]
	];
	
	exclusive = $PutOnShell[$Get[integrated, {"integrated", "long"}]]
		/. {eps^2 -> 0}
		/. {k.n -> x, n.p -> 1, n.q -> 1-x};
	
	If[
		$debug
		,
		Module[{t$ir$k, t$uv$k, t$uv$p, t$uv$q},
			t$uv$k = PolePart[
				Expand[exclusive]
					/. {p.p->0, q.q->0}
					/. {0^(-eir)->0, 0^(_-eir):>0}
				,
				euv
			];
			t$uv$p = PolePart[
				Expand[exclusive k.k]
					/. {k.k->0, q.q->0}
					/. {0^(-eir)->0, 0^(_-eir):>0}
				,
				euv
			] / k.k;
			t$uv$q = PolePart[
				Expand[exclusive k.k]
					/. {k.k->0, p.p->0}
					/. {0^(-eir)->0, 0^(_-eir):>0}
				,
				euv
			] / k.k;
			
			DebugInfo[
				"SplittingFunction::T_UV^k"
				,
				Collect[t$uv$k, {I0, Log[x]}, Simplify]
			];
			DebugInfo[
				"SplittingFunction::T_UV^p"
				,
				Collect[t$uv$p, {I0, Log[x]}, Simplify]
			];
			DebugInfo[
				"SplittingFunction::T_UV^q"
				,
				Collect[t$uv$q, {I0, Log[x]}, Simplify]
			];

			t$ir$k = PolePart[Expand[exclusive], eir];
			DebugInfo[
				"SplittingFunction::T_IR^k"
				,
				Collect[t$ir$k, {I0, Log[x]}, Simplify]
			];
			
			DebugInfo[
				"SplittingFunction::T_UV^p + T_UV^q - T_IR^k"
				,
				Collect[
					Expand[t$uv$p + t$uv$q - t$ir$k]
						/. {eir->0}
						/. {0^-eir :> 1}
					,
					{I0, Log[x]}
					,
					Simplify
				]
			];		
		]
	];
	
	Z = Simplify[If[
		SameQ[$LO, Null]
		,
		0
		,
		PolePart[
			exclusive
				(* /. {(k.k)^(n_Integer-eir) :> (k.k)^n} *)
				/. {S[_,_]^(-eir) :> 1}
			,
			euv
		] / $Get[$LO, "exclusive"]
	] /. {eir -> 0, eps -> 0}];

	counterterm = If[
		SameQ[$LO, Null]
		,
		0
		,
		2^(2 eir) Pi^(eir) Gamma[1+eir] Z $Get[$LO, "exclusive"]
	];
	
	exclusive = exclusive - counterterm / euv
		/. {eir -> - eps, euv -> -eps}
		/. {p.p -> 0, q.q -> 0}
		/. {0^eps -> 0}
	;

	inclusive = Simplify[PolePart[
		IntegrateFinal[exclusive, 4 + 2 eps]
		,
		eps
	]];

	{
		{"trace", trace},
		{"integrated", integrated},
		{"Z", Z},
		{"counterterm", counterterm},
		{"exclusive-bare", exclusiveBare},
		{"exclusive", exclusive},
		{"inclusive", inclusive}
	}
];


DebugInfo[
	"Axiloop"
	,
	"Exiting AXILOOP"
];

End[]


EndPackage[]
