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

ExtractPole::usage =
	"ExtractPole[expr, x] extract coefficient in front of 1/x in expr."

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

DebugInfo::log = "`1`";

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
    (-x_Symbol).y_Symbol := -x.y;
Protect[Dot];


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

IntegrateFinal[kernel_, ndim_:4 + 2 eps] := Module[{eps},
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

	result
];


SimplifyLoopIntegrals[expr_] := Module[
	{result, signCorrectionRules, simplifyRules},
		
	signCorrectionRules = {
		$$[{a___}, {b___}, {c___}] :>
			(-1)^(Length[{a}]+Length[{c}]) $$[{a}, -# &/@ {b}, -# &/@ {c}] /;
				{b} == Select[{b}, Or[# == 0, MatchQ[#, Times[-1, _Symbol]]] &]
	};

	simplifyRules = {
		(*
		$$[{a1___,x_,a2___},{0,b1___,x_,b2___},{c___}] :> 1/2 (
			$$[{a1,a2}, {0,b1,b2}, {c}] - $$[{a1,a2}, {b1,x,b2}, {c}] - $$[{a1,a2}, {0,b1,x,b2}, {c}] x.x
		)
		*)
		$$[{k,k}, {0,k,p}, {0}] -> 1/2 (
			$$[{k}, {0,p}, {0}] - $$[{k}, {k,p}, {0}] - $$[{k}, {0,k,p}, {0}] k.k
		)
		,
		$$[{k,p}, {0,k,p}, {0}] -> 1/2 (
			$$[{p}, {0,p}, {0}] - $$[{p}, {k,p}, {0}] - $$[{p}, {0,k,p}, {0}] k.k
		)
		,
		$$[{p,p}, {0,k,p}, {0}] -> 1/2 (
			$$[{p}, {0,k}, {0}] - $$[{p}, {k,p}, {0}] - $$[{p}, {0,k,p}, {0}] p.p
		)
		,
		
		$$[{k}, {0,k,p}, {0}] -> 1/2 (
			$$[{}, {0,p}, {0}] - $$[{}, {k,p}, {0}] - $$[{}, {0,k,p}, {0}] k.k
		)
		,
		$$[{p}, {0,k,p}, {0}] -> 1/2 (
			$$[{}, {0,k}, {0}] - $$[{}, {k,p}, {0}] - $$[{}, {0,k,p}, {0}] p.p
		)
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

	k.n -> x,
	n.p -> 1,
	n.q -> 1-x,

	n.n -> 0
};

$onShellRules = {
	p.p -> 0,
	q.q -> 0
}

IntegrateLoop::unevaluated = "Unknown integral(s): `1`."

IntegrateLoop[expr_, l_] := Module[
	{collected, expansionRules, integratedLong, integratedShort,
	 integrationRules, phaseSpaceRule, simplified, unevaluated},
	
	integrationRules = {
		$$[{},{0,k},{ }] ->   Q (k.k)^(-eir) T0,
		$$[{},{0,p},{ }] ->   Q (p.p)^(-eir) T0,
		$$[{},{k,p},{ }] ->   Q (q.q)^(-eir) T0,

		$$[{x_},{0,k},{}] :> - Q (k.k)^(-eir) k.x T1,
		$$[{x_},{0,p},{}] :> - Q (p.p)^(-eir) p.x T1,
		$$[{x_},{0,q},{}] -> - Q (q.q)^(-eir) q.x T1,
		$$[{x_},{k,p},{}] :> - Q (q.q)^(-eir) (q.x T1 - k.x T0),
		
		$$[{},{0,k,p},{}] -> - Q (k.k)^(-1-eir) R0,
		$$[{x_},{0,k,p},{}] :> Q (k.k)^(-1-eir) (p.x R1 + k.x R2),
		$$[{x_, y_},{0,k,p},{}] :> Q (k.k)^(-1-eir) (
    		p.x p.y R3 + k.x k.y R4 + (k.x p.y + p.x k.y) R5 + k.k x.y R6
   		),
   

		$$[{},{0,k},{0}] -> - Q (k.k)^(-eir) P0 / k.n,
		$$[{},{0,p},{0}] -> - Q (p.p)^(-eir) B0 / n.p,
		$$[{},{k,p},{0}] -> - Q (q.q)^(-eir) K0 / n.p,
		
		$$[{x_},{0,k},{0}] :> Q (k.k)^(-eir)/k.n (
			k.x P1 + n.x k.k/(2 k.n) P3
		),
		$$[{x_},{0,p},{0}] :> Q (p.p)^(-eir)/p.n (
			p.x B1 + n.x p.p/(2 p.n) B3
		),
		$$[{x_},{k,p},{0}] :> Q (q.q)^(-eir) (
			p.x V1 + k.x V2 + n.x q.q V3
		),
		
		$$[{},{0,k,p},{0}] -> Q (k.k)^(-1-eir)/p.n S0,
	
		$$[{x_},{0,k,p},{0}] :> - Q (k.k)^(-1-eir)/p.n (
			p.x S1 + k.x S2 + n.x k.k/(2 k.n) S3
		)
	};
	
	phaseSpaceRule = {
		Q -> I (4 Pi)^(-2+eir) Gamma[1+eir]
	};
	
	expansionRules = {
		B0 -> I0/euv - I1 + Li2[1],
		B1 -> 1/euv + 2,
		B3 -> (I0 - 2)/euv - I1 - 4 + Li2[1],

		K0 -> - Log[x]/((1-x) euv),

		P0 -> (I0 + Log[x])/euv - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
		P1 -> 1/euv + 2,
		P3 -> (I0 + Log[x] - 2)/euv - I1 + I0 Log[x] + (Log[x]^2)/2 - 4 + Li2[1],
	
		R0 ->  1/eir^2 - Li2[1],
		R1 ->  1/eir^2 + 2/eir + 4 - Li2[1],
		R2 -> -1/eir - 2,
		R3 ->  1/eir^2 + 3/eir + 7 - Li2[1],
		R4 -> -1/(2 eir) - 1,
		R5 -> -1/(2 eir) - 3/2,
		R6 ->  1/(4 euv) + 3/4,
		
		S0 -> 1/eir^2 - (I0 - Log[x])/eir + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x] - (Log[x]^2)/2,
		S1 -> 1/eir^2 - x Log[x]/((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
		S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x),
		
		T0 -> 1/euv + 2,
		T1 -> 1/(2 euv) + 1,

		V1 -> (1-x + x Log[x])/(1-x)^2 / euv,
		V2 -> - (1-x + Log[x])/(1-x)^2 / euv
	};
	
	collected = CollectLoopIntegrals[expr, l];
	
	simplified = SimplifyLoopIntegrals[collected];

	integratedShort = Expand[
		simplified
			/. integrationRules
			/. phaseSpaceRule
			/. $kinematicRules
	];
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


ExtractPole[kernel_, eta_] := Expand[
	Coefficient[Series[kernel, {eta, 0, 1}], eta, -1]
];


(*---------------------------------------------------------------------------*)
(*--------------------------- SPLITTING FUNCTION ----------------------------*)
(*---------------------------------------------------------------------------*)

SplittingFunction[topology_] := Module[
	{counterterm, exclusive, integrated, trace},
	
	trace = Expand[
		GammaTrace[topology, NumberOfDimensions -> 4 + 2 eps]
			/. $kinematicRules
	];

	exclusive = IntegrateLoop[trace, l];
	integrated = $Get[exclusive, {"integrated", "long"}]; 
	If[
		integrated == Null
		,
		Return[Null]
	];

	counterterm = ExtractPole[
		integrated
			/. {eps -> 0}
			/. $onShellRules
			/. {0^(-eir) -> 1, 0^(n_Integer-eir) :> 0 /; n>0}
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
		,
		euv
	]; 

	{
		{"trace", trace},
		{"exclusive", exclusive},
		{"counterterm", counterterm}
	}
];


DebugInfo[
	"Axiloop"
	,
	"Exiting AXILOOP"
];

End[]


EndPackage[]
