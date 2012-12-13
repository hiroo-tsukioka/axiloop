(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012 Oleksandr Gituliar.                                    *)
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

GammaTrace::usage = "GammaTrace[expr, NumberOfDimensions -> 4 - 2 eps] computes
a trace of the gamma matrices product, `expr`, in the arbitrary
number of dimensions, `NumberOfDimensions`.

Usage:
    In[1] := GammaTrace[G[{mu}]**G[{nu}], NumberOfDimensions -> 4 + eps]
    Out[1] = 4 (4 + eps)"

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

GetValue::usage =
	"GetValue[kernel_, key_] get value from kernel associated with key."

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

Counterterm::usage = ""


Begin["`Private`"]

Print["Entering AXILOOP..."];


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)

DEBUG = True;

Unprotect[Debug];
	Debug[label_, expr_] := If[DEBUG, Print[Row[{label, expr}, " = "]]];
Protect[Debug];

(* Useful modifications to standard functions *)

Unprotect[Dot];
    (-x_).y_ := - x.y;
    (*
    x_.(-y_) := - x.y;
    (x_.(-x_))^2 := (x.x)^2;
    *)
Protect[Dot];

Unprotect[ReplaceRepeated];
	ReplaceRepeated[expr_,{{}}] := expr;
	ReplaceRepeated[expr_,{rules__List}] := ReplaceRepeated[expr //. First[{rules}], Rest[{rules}]];
Protect[ReplaceRepeated];

GetValue[kernel_, key_, default_:0] := If[
	!ListQ[kernel] || Equal[kernel, {}],
	default,
	Module[{match},
		match = Select[kernel, First[#] == key &, 1];
		If[
			match == {},
			default,
			Last[First[match]]
		]
	]
];

(* Kinematics definition and some transformations. *)

Unprotect[S];
    S[n,n] = 0;
Protect[S];


(*---------------------------------------------------------------------------*)
(*---------------------- FEYNMAN RULES and GAMMA TRACE ----------------------*)
(*---------------------------------------------------------------------------*)

FermionLines = {};    (* A storage of fermion lines used by user. Is used    *)
                      (* in `GammaTrace` as a list of lines to trace over.   *)

Options[G] = {Line -> f1};
G[v_, OptionsPattern[]] := (
	FermionLines = Union[FermionLines, {OptionValue[Line]}];
	GTrace[OptionValue[Line], v]
);

Options[FP] = {Line -> f1};
FP[p_, OptionsPattern[]] := FPx[p, Line -> OptionValue[Line]] / p.p;

Options[FPx] = {Line -> f1};
FPx[p_, OptionsPattern[]] := I G[p, Line -> OptionValue[Line]];

Options[FV] = {Line -> f1};
FV[mu_, OptionsPattern[]] := - I g G[{mu}, Line -> OptionValue[Line]];

GP[mu_, nu_, p_] := GPx[mu, nu, p] / p.p;

GPx[mu_, nu_, p_] := - I ({mu}.{nu} - (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n)

GV[i1_,p1_, i2_,p2_, i3_,p3_] :=
	g ( {i1}.{i2} (p1.{i3}-p2.{i3})
	  + {i2}.{i3} (p2.{i1}-p3.{i1})
	  + {i3}.{i1} (p3.{i2}-p1.{i2})
);

Options[GammaTrace] = {NumberOfDimensions -> 4 - 2 eps};
GammaTrace[expr_, OptionsPattern[]] := Module[
	{ndim = OptionValue[NumberOfDimensions], result},
	
	VectorDimension[ndim];
	Spur[f0];
	result = Expand[expr //. {
		((#->f0)& /@ FermionLines),
		{Global`d -> ndim, NonCommutativeMultiply -> Times}
	}];
	NoSpur[f0];
	(*VectorDimension[Global`d];*)
	Return[result];
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

CollectIntegralRules[l_] := {
	KK[l, {x___},{y___},{z___}] S[l,p_] :>
		KK[l, {p,x},{y},{z}],
	KK[l, {x___},{y___},{z___}] Power[S[l,p_], n_] :>
		KK[l, {p,x},{y},{z}] Power[S[l,p], n-1] /; n>0,

	KK[l, {x___},{y___},{z___}] / S[l,l] :>
		KK[l, {x},{0,y},{z}],
	KK[l, {x___},{y___},{z___}] / S[l-l1_,l-l1_] :>
		KK[l, {x},{-l1,y},{z}],
	KK[l, {x___},{y___},{z___}] / S[l+l1_,l+l1_] :>
		KK[l, {x},{l1,y},{z}],

	KK[l, {x___},{y___},{z___}] / S[l,n] :>
		KK[l, {x},{y},{0,z}],
	KK[l, {x___},{y___},{z___}] / (S[l,n]+S[ln_,n]) :>
		KK[l, {x},{y},{ln,z}],
	KK[l, {x___},{y___},{z___}] / (S[l,n]-S[ln_,n]) :>
		KK[l, {x},{y},{-ln,z}],
	KK[l, {x___},{y___},{z___}] / S[l+ln_,n] :>
		KK[l, {x},{y},{ln,z}],
	KK[l, {x___},{y___},{z___}] / (S[ln_,n]-S[l,n]) :>
		- KK[l, {x},{y},{-ln,z}],

	KK[l, {},{},{}] -> 1
};

CollectIntegral[expr_, l_] := Module[{},
	Expand[expr * KK[l, {},{},{}], l] //. CollectIntegralRules[l]
];


ExpandIntegralRules = {
	{
		KK[l_, {x1_,x___},{y___},{z___}] :> x1.l KK[l, {x},{y},{z}],
		KK[l_, {x___},{y1_,y___},{z___}] :> KK[l, {x},{y},{z}] / (l+y1).(l+y1),
		KK[l_, {x___},{y___},{z1_,z___}] :> KK[l, {x},{y},{z}] / (l+z1).n,
		KK[l_, {},{},{}] -> 1
	}
};
ExpandIntegral[expr_] := Module[{},
	Expand[expr //. ExpandIntegralRules]
];


ReduceIntegralRulesNew[l_] := {
	{
		KK[l, {}, {-k,0}, {}] -> KK[l, {}, {k,0}, {}],
		KK[l, {}, {-p,-k}, {}] -> KK[l, {}, {p,k}, {}],
		KK[l, {}, {-p,-k,0}, {}] -> KK[l, {}, {p,k,0}, {}],
		KK[l, {x_}, {-k,0}, {}] -> - KK[l, {x}, {k,0}, {}],
		KK[l, {x_}, {-p,-k,0}, {}] -> - KK[l, {x}, {p,k,0}, {}],
		KK[l, {x_,y_}, {-p,-k,0}, {}] :> KK[l, {x,y}, {p,k,0}, {}],
		
		KK[l, {}, {-k}, {0}] -> - KK[l, {}, {k}, {0}],
		
		KK[l, {}, {-p,-k}, {0}] :> - KK[l, {}, {p,k}, {0}],
		KK[l, {x_}, {-k,0}, {0}] -> KK[l, {x}, {k,0}, {0}],
		KK[l, {x_}, {-p,-k}, {0}] :> KK[l, {x}, {p,k}, {0}],
		KK[l, {x_}, {-p,-k,0}, {0}] -> KK[l, {x}, {p,k,0}, {0}],
		KK[l, {x_,y_}, {-p,-k,0}, {0}] :> - KK[l, {x,y}, {p,k,0}, {0}]
	},
	
	{
		KK[l, {p,k}, {p,k,0}, {0}] ->
		    - 1/2 ( KK[l, {k}, {p,k}, {0}]
		    	+ KK[l, {k}, {p,k,0}, {0}] p.p
		    	- KK[l, {k}, {k,0}, {0}]
		    ),
		KK[l, {p,p}, {p,k,0}, {0}] ->
		    - 1/2 ( KK[l, {p}, {p,k}, {0}]
		    	+ KK[l, {p}, {p,k,0}, {0}] p.p
		    	- KK[l, {p}, {k,0}, {0}]
		    ),
		KK[l, {k,k}, {p,k,0}, {0}] ->
		    - 1/2 ( KK[l, {k}, {p,k}, {0}]
		    	+ KK[l, {k}, {p,k,0}, {0}] k.k
		    	- KK[l, {k}, {p,0}, {0}]
		    )
	}
	(*    
		KK[l, {p}, {p,k}, {0}] -> 
			1/2 ( KK[l, {}, {p}, {0}]
				- KK[l, {}, {k}, {0}]
				+ 2 KK[l, {k}, {p,k}, {0}]
				+ (p.p-k.k) KK[l, {}, {p,k}, {0}]
			),
		KK[l, {}, {p}, {0}] -> (
			KK[l, {}, {0}, {0}]
			+ 2 KK[l, {p}, {p,0}, {0}]
			- p.p KK[l, {}, {p,0}, {0}]
		),
		KK[l, {}, {k}, {0}] -> (
			KK[l, {}, {0}, {0}]
			+ 2 KK[l, {k}, {k,0}, {0}]
			- k.k KK[l, {}, {k,0}, {0}]
		),
		
		KK[l, {}, {p,k}, {0}] -> 1/(k.k-p.p) (
			KK[l, {}, {p}, {0}]
			- KK[l, {}, {k}, {0}]
			- 2 KK[l, {p}, {p,k}, {0}]
			+ 2 KK[l, {k}, {p,k}, {0}]
		)
	}
	*)
};

ReduceIntegralRules[l_] := {
	ReduceIntegralRulesNew[l][[1]],
	{
		KK[l, {x1___,l,x2___},{y1___,0,y2___},{z___}] :>
            KK[l, {x1,x2},{y1,y2},{z}],

		KK[l,{x1___,a_,x2___},{y1___,a_,y2___},{z___}] :>
			( KK[l, {x1,x2},{y1,y2},{z}]
			- KK[l, {x1,x2},{y1,a,y2},{z}] a.a
			- KK[l, {x1,l,x2},{y1,a,y2},{z}]) / 2
		(*
		KK[l, {x___},{y___},{z1___,z2_,z3_,z4___}] :>
			(KK[l, {x},{y},{z1,z2,z4}] - KK[l, {x},{y},{z1,z3,z4}]) / (z2.n-z3.n),

		KK[l, {x1___,n,x2___},{y___},{z1___,z_,z2___}] :>
			KK[l, {x1,x2},{y},{z1,z2}] + KK[l, {x1,x2},{y},{z1,z,z2}] z.n
		*)
	}, {
		KK[l, {l},{y1___,y_,y2___},{z___}] :>
			( KK[l, {},{y1,y2},{z}]
			- 2 KK[l, {y},{y1,y,y2},{z}]
			- y.y KK[l, {},{y1,y,y2},{z}] )
		(*
		KK[l, {},{y___},{p_Symbol}] :>
			KK[l, {},(#-p)&/@{y},{0}],

		KK[l, {x_Symbol},{y___},{p_Symbol}] :>
			KK[l, {x},(#-p)&/@{y},{0}] + x.p KK[l, {},(#-p)&/@{y},{0}]
		*)
	}, {
		KK[l, {},{y_Symbol},{0}] :>
			KK[l, {},{0},{0}] - 2 KK[l, {y},{y,0},{0}] - y.y KK[l, {},{y,0},{0}]
	}
};

ReduceIntegral[expr_, l_] := Module[{},
	Expand[expr //. ReduceIntegralRules[l]]
];

IntegrateLoopRulesNull[l_] := {};

IntegrateLoopRules = {
	(* I2 *)
	(*
	K[{},{p,0},{}] -> Q (p.p)^(-eir) T0,
	K[{},{k,0},{}] -> Q (k.k)^(-eir) T0,
	*)
	K[{},{k,p},{}] -> Q (q.q)^(-eir) T0,
	(*
	(* I2x *)
	K[{p},{k,0},{}] -> - Q (k.k)^(-eir) k.p T1,
	K[{k},{k,0},{}] -> - Q (k.k)^(-eir) k.k T1,
	K[{n},{k,0},{}] -> - Q (k.k)^(-eir) k.n T1,
	K[{q},{k,0},{}] -> - Q (k.k)^(-eir) k.q T1,
	K[{p},{p,0},{}] -> - Q (p.p)^(-eir) p.p T1,
	K[{k},{p,0},{}] -> - Q (p.p)^(-eir) k.p T1,
	K[{n},{p,0},{}] -> - Q (p.p)^(-eir) n.p T1,
	K[{p},{q,0},{}] -> - Q (q.q)^(-eir) p.q T1,
	K[{k},{q,0},{}] -> - Q (q.q)^(-eir) k.q T1,
	K[{x_},{p,k},{}] :>  K[{x},{q,0},{}] - K[{k},{q,0},{}],
	*)
	(* I3 *)
	K[{},{p,k,0},{}] -> - Q (k.k)^(-1-eir) R0,
	K[{},{k,p,0},{}] -> K[{},{p,k,0},{}],
	
    (* I3x *)
    K[{x_},{k,p,0},{}] :> Q (k.k)^(-1-eir) (p.x R1 + k.x R2),
    
    (* I3xy *)
    K[{x_, y_},{k,p,0},{}] :> Q (k.k)^(-1-eir) (
    	p.x p.y R3 + k.x k.y R4
    	+ (k.x p.y + p.x k.y) R5
    	+ k.k x.y R6
    ),
	
	(* K2 *)
	K[{},{k,0},{0}] -> - Q (k.k)^(-eir) P0 / k.n,
	K[{},{p,0},{0}] -> - Q (p.p)^(-eir) B0 / p.n,
	K[{},{k,p},{0}] -> - Q (q.q)^(-eir) K0 / p.n,
	
	(* K2x *)
	K[{q},{k,0},{0}] -> K[{p},{k,0},{0}] - K[{k},{k,0},{0}],
	K[{k},{p,0},{0}] -> Q (p.p)^(-eir)/p.n (
		k.p B1 + k.n p.p/(2 p.n) B3
	),
	K[{p},{p,0},{0}] -> Q (p.p)^(-eir)/p.n (
		p.p B1 + p.n p.p/(2 p.n) B3
	),
	K[{n},{p,0},{0}] -> Q (p.p)^(-eir)/p.n (
		p.n B1 + n.n p.p/(2 p.n) B3
	),
	
	
	K[{x_},{k,0},{0}] :> Q (k.k)^(-eir)/k.n (
		k.x P1 + n.x k.k/(2 k.n) P3
	),
	(*
	K[{x_},{p,k},{0}] -> Q (q.q)^(-eir) (
		x.p V1 + x.k V2
	),
	*)
	(* K3 *)
	K[{},{p,k,0},{0}] -> Q (k.k)^(-1-eir)/p.n S0,
	K[{},{k,p,0},{0}] -> K[{},{p,k,0},{0}],
	
	(* K3x *)
	K[{k},{k,p,0},{0}] -> - Q (k.k)^(-1-eir)/p.n (
		k.p S1 + k.k S2 + k.n k.k/(2 k.n) S3
	),
	K[{p},{k,p,0},{0}] -> - Q (k.k)^(-1-eir)/p.n (
		p.p S1 + p.k S2 + p.n k.k/(2 k.n) S3
	),
	K[{n},{k,p,0},{0}] -> - Q (k.k)^(-1-eir)/p.n (
		p.n S1 + k.n S2 + n.n k.k/(2 k.n) S3
	),
	
	Q -> I (4 Pi)^(-2+eir) Gamma[1+eir]
};

IntegrateLoopExpandRules = {
	
	B0 -> I0/euv - I1 + Li2[1],
	B1 -> 1/euv + 2,
	
	B3 -> (I0 - 2) / euv - 4 - I1 + Li2[1],
	
	C0 -> (Log[1-x] + I0) / euv - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1],
	C1 -> 1 / euv + 2,
	C3 -> (I0 + Log[1-x] - 2) / euv - 4 - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1],
	D0 -> (Log[1-x] - Log[x]) / euv + (Log[x]^2)/2 - (Log[1-x]^2)/2 + Li2[1] - 2 Li2[1-x] - Log[x]Log[1-x],
	K0 -> - Log[x] / ((1-x) euv),
	P0 -> (Log[x] + I0) / euv - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
	P1 -> 1/euv + 2,
	
	P3 -> (Log[x] + I0 - 2) / euv - 4 - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
	
	R0 -> 1/eir^2 - Li2[1],
	R1 -> 1/eir^2 + 2/eir + 4 - Li2[1],
	R2 -> -1/eir - 2,
	R3 -> 1/eir^2 + 3/eir + 7 - Li2[1],
	R4 -> -1/(2 eir) - 1,
	R5 -> -1/(2 eir) - 3/2,
	R6 -> 1/(4 euv) + 3/4,
	S0 -> 1/eir^2 + (Log[x] - I0) / eir + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x] - (Log[x]^2)/2,
	S1 -> 1/eir^2 - x Log[x] / ((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
	S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x),

	S3 -> - ((I0 + Log[x]/(1-x)) / eir - I1 + I0 Log[x]/(1-x) - Li2[1] - x/(1-x) Li2[1-x] + (Log[x]^2)/2),

	T0 -> 1/euv + 2,
	T1 -> 1/(2 euv) + 1,
	
	V1 -> (x Log[x] + 1-x)/(1-x)^2 / euv,
	V2 -> - (Log[x] + 1-x)/(1-x)^2 / euv,
	
	U0 -> 1/eir^2 + (Log[x] - 2 Log[1-x] - I0) / eir + I1 - I0 Log[x] + 2 Li2[1-x] - (Log[x]^2)/2 + Log[1-x]^2 - 6 Li2[1]
	
}

Options[IntegrateLoop] = {Compact -> False};
IntegrateLoop[kernel_, l_, OptionsPattern[]] := Module[
	{compact, expanded, reduced},
	
	reduced = ReduceIntegral[CollectIntegral[kernel, l], l]
	    //. KK[l, xyz___] -> K[xyz];
	(*
	reduced = CollectIntegral[kernel, l]
	    //. KK[l, xyz___] -> K[xyz];
	*)
	Debug["IntegrateLoop.ReduceIntegral", reduced];

	compact = Expand[reduced //.IntegrateLoopRules[l]];
	Debug["IntegrateLoop.IntegrateLoopRules", compact];
	(* Pretty[compact, {P0, P1, R0, R1, R2, R3, R4, R5, S0, T0}] *)

	If[OptionValue[Compact], Return[compact]];

	expanded = Simplify[compact /. IntegrateLoopExpandRules];
	Return[expanded];
];


(*---------------------------------------------------------------------------*)
(*------------------------ CONTRACT INDICES FUNCTION ------------------------*)
(*---------------------------------------------------------------------------*)

ContractIndicesRules = {
	
	k.{i1_} K[{{i1_}},{y__},{z___}] :> K[{k},{y},{z}],
	p.{i1_} K[{{i1_}},{y__},{z___}] :> K[{p},{y},{z}],
	n.{i1_} K[{{i1_}},{y__},{z___}] :> K[{n},{y},{z}],
	
	k.{i1_} k.{i2_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{k,k}, {y}, {z}],
	k.{i1_} p.{i2_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{k,p}, {y}, {z}],
	k.{i1_} n.{i2_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{k,n}, {y}, {z}],
	k.{i2_} n.{i1_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{k,n}, {y}, {z}],
	p.{i1_} p.{i2_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{p,p}, {y}, {z}],
	p.{i1_} n.{i2_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{p,n}, {y}, {z}],
	p.{i2_} n.{i1_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{p,n}, {y}, {z}],
	n.{i1_} n.{i2_} K[{{i1_},{i2_}}, {y__}, {z___}] :> K[{n,n}, {y}, {z}],
	{i1_}.{i2_} K[{{i1_},{i2_}}, {k,p,0}, {0}] :> K[{}, {k,p}, {0}],
	
	K[{n}, {k,p,0}, {0}] :> K[{}, {k,p,0}, {}],
	K[{x_,n}, {k,p,0}, {0}] :> K[{x}, {k,p,0}, {}]

};

ContractIndices[expr_] := expr //. ContractIndicesRules;

(*---------------------------------------------------------------------------*)
(*------------------------- PARTON DENSITY FUNCTION -------------------------*)
(*---------------------------------------------------------------------------*)

PartonDensity[topology_, LO_:0] := Module[
	{ExclusiveLO, ExclusiveNLO, InclusiveNLO, KernelNLO, Z},
	
	KernelNLO = Expand[
		ContractIndices[
			GammaTrace[topology, NumberOfDimensions -> 4 + 2 eps]
				//. {KinematicRules}
		]
	];
	(*Debug["kernel", KernelNLO];*)
	
	TT = Expand[
		Simplify[KernelNLO //. {IntegrateLoopRules}]
	] //. {ScalarProductRules};
	Debug["T", Collect[
		Expand[TT / (Gamma[1+eir] g^4 Pi^(-2+eir) 2^(2 eir))] //.
		    {OnShellRules, {0^(2-eir)->0, 0^(1-eir)->0}},
		{B0, B1, K0, P0, P1, R0, R1, R2, R3, R4, R5, R6, S0, S1, S2, S3, T0, T1},
		Simplify
	]];
	
	ExclusiveNLO = Expand[TT //. {IntegrateLoopExpandRules, ScalarProductRules}];
	ExclusiveLO = GetValue[LO, "exclusive"];
	
	OnShellNLOPole = Simplify[
		Expand[ExtractPole[ExclusiveNLO, euv]] //.
			{{eps->0, eir->0}, OnShellRules}
	];
	OnShellExclusiveLO = Simplify[ExclusiveLO //. {{eps->0}, OnShellRules}];
	Z =	If[SameQ[ExclusiveLO, 0],
		0,
		Simplify[OnShellNLOPole / OnShellExclusiveLO]
	];
	Debug["Z", Z];
	Debug["DoublePole", Simplify[
		Coefficient[
			Series[ExclusiveNLO //. {eps -> -eir}, {eir, 0, 1}],
			eir,
			-2
		] //. {OnShellRules}
	]];
	
	
	v0 = Expand[
		ExclusiveNLO - 2^(2 eir) Pi^(eir) Gamma[1+eir] Z ExclusiveLO / (euv)
	];
	(*Debug["v0", v0];*)
	v1 = Expand[
		v0 //. {eir -> -eps, euv -> -eps}
	];
	(*Debug["v1", v1];*)
	v2 = Expand[v1 //. {OnShellRules, {0^(eps)->0, 0^(1+eps)->0, 0^(2+eps)->0}}];
	Debug["v2", v2];
	v3 = Expand[IntegrateFinal[v2, 4 + 2 eps]];
	Debug["v3", v3];
	v4 = ExtractPole[v3, eps];
	Debug["v4", v4];

	
	OnShellExclusiveLO = ExclusiveLO //. OnShellRules;
	OnShellExclusiveNLO = ExclusiveNLO //.
		{{eir -> -eps}, OnShellRules, {0^(eps)->0, 0^(1+eps)->0, 0^(2+eps)->0}};
	v1 = IntegrateFinal[
		OnShellExclusiveNLO + (Z OnShellExclusiveLO) / eps, 4 + 2 eps
	];
	v2 = ExtractPole[v1, eps] //. {ScalarProductRules};
	InclusiveNLO = Collect[
		Expand[Simplify[v4]],
		{
			(Log[x])^2, Log[x] Log[1 - x], x Log[x], I0 Log[x], I0 Log[1 - x],
			Log[x], Log[1 - x], I0, I1, Li2[1]
		},
		Simplify];
	
	{
		{"kernel", KernelNLO},
		{"exclusive", ExclusiveNLO (* IntegrateLoop[KernelNLO, l, Compact->True] //. {ScalarProductRules}*)},
		{"inclusive", InclusiveNLO},
		{"Z", Z}
	}
];

(* Renormalization routines and helpers *)

OnShellRules = {
	p.p -> 0,
	q.q -> 0
};

KinematicRules = {
	k.p -> (p.p+k.k-q.q)/2,
	k.q -> (p.p-k.k-q.q)/2,
	p.q -> (p.p-k.k+q.q)/2
};

ScalarProductRules = {
	k.p -> (p.p+k.k-q.q)/2,
	k.q -> (p.p-k.k-q.q)/2,
	p.q -> (p.p-k.k+q.q)/2,
	
	n.q -> n.p - n.k,
	n.p -> 1,
	n.k -> x
};


(*---------------------------------------------------------------------------*)
(*-------------------------- COUNTERTERM FUNCTION ---------------------------*)
(*---------------------------------------------------------------------------*)

Counterterm[ExclusiveNLO_, ExclusiveLO_, eta_] := Module[{},
	OnShellPole = ExtractPole[ExclusiveNLO, eta] //.
		{{eps->0}, OnShellRules, {0^-eir->0, 0^(1-eir)->0, 0^(2-eir)->0}, {eir->0}};
	OnShellExclusiveLO = ExclusiveLO //.
		{OnShellRules};
	Debug["OnShellPole", OnShellPole];

	If[ SameQ[ExclusiveLO, 0],
		0,
		OnShellPole / OnShellExclusiveLO
	]
];

ExtractPole[kernel_, eta_] := Simplify[
	Coefficient[Series[kernel, {eta, 0, 1}], eta, -1]
];

Pretty[expr_, matches_:{}] := Module[{defaults},
	defaults = {
		Pi^eta,
		4^eta,
		(4 Pi)^eta,
		(k.k)^(-1-eta),
		Gamma[1+eta],
		g^4
	}; 
	
	Collect[expr, Join[defaults, matches] , Simplify]
];


Print["Exiting AXILOOP..."];

End[]


EndPackage[]
