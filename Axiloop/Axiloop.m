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

SplittingFunction::usage = ""


Begin["`Private`"]

Print["Entering AXILOOP..."];


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)

DEBUG = True;

Unprotect[Debug];
	Debug[label_, expr_] := If[DEBUG, Print[Row[{label, expr}, " = "]]];
Protect[Debug];

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

(*
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
*)


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
        KK[l, x_, y_, z_] :> - KK[l, x, -# & /@ y, z] /; 
            OddQ[Length[x] + Length[z]],
        KK[l, x_, y_, z_] :> K[x, -# & /@ y, z]
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
	K[{},{p,q},{}] -> K[{},{k,0},{}],
	K[{},{p,0},{}] -> Q (p.p)^(-eir) T0,
	K[{},{k,0},{}] -> Q (k.k)^(-eir) T0,
	K[{},{k,p},{}] -> Q (q.q)^(-eir) T0,

	(* I2x *)
	K[{p},{k,0},{}] -> - Q (k.k)^(-eir) k.p T1,
	K[{k},{k,0},{}] -> - Q (k.k)^(-eir) k.k T1,
	K[{n},{k,0},{}] -> - Q (k.k)^(-eir) k.n T1,
	(*
	K[{q},{k,0},{}] -> - Q (k.k)^(-eir) k.q T1,
	K[{p},{p,0},{}] -> - Q (p.p)^(-eir) p.p T1,
	K[{k},{p,0},{}] -> - Q (p.p)^(-eir) k.p T1,
	K[{n},{p,0},{}] -> - Q (p.p)^(-eir) n.p T1,
	K[{p},{q,0},{}] -> - Q (q.q)^(-eir) p.q T1,
	K[{k},{q,0},{}] -> - Q (q.q)^(-eir) k.q T1,
	K[{x_},{p,k},{}] :>  K[{x},{q,0},{}] - K[{k},{q,0},{}],
	*)
	(* I3 *)
	K[{},{p,q,0},{}] -> K[{},{k,p,0},{}],
	K[{},{p,k,0},{}] -> - Q (k.k)^(-1-eir) R0,
	K[{},{k,p,0},{}] -> K[{},{p,k,0},{}],
	
    (* I3x *)
    K[{x_},{p,q,0},{}] :> (
    	- K[{x},{k,p,0},{}]
    	- x.p K[{},{k,p,0},{}]
    ),
    K[{x_},{k,p,0},{}] :> Q (k.k)^(-1-eir) (p.x R1 + k.x R2),
    
    (* I3xy *)
    K[{x_, y_},{p,q,0},{}] :> (
    	K[{x, y},{k,p,0},{}]
    	+ x.p K[{y},{k,p,0},{}]
    	+ y.p K[{x},{k,p,0},{}]
    	+ x.p y.p K[{},{k,p,0},{}]
    ),
    K[{x_, y_},{k,p,0},{}] :> - Q (k.k)^(-1-eir) (
    	p.x p.y R3 + k.x k.y R4
    	+ (k.x p.y + p.x k.y) R5
    	+ k.k x.y R6
    ),
	
	(* K1 *)
	K[{},{0},{0}] -> 0,
	K[{},{k},{0}] -> K[{},{0},{0}] - 2 K[{k},{k,0},{0}] - k.k K[{},{k,0},{0}],
	
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
	
	B3 -> (I0 - 2)/euv - I1 - 4 + Li2[1],
	
	C0 -> (Log[1-x] + I0)/euv - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1],
	C1 -> 1/euv + 2,
	C3 -> (I0 + Log[1-x] - 2)/euv - 4 - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1],
	D0 -> (Log[1-x] - Log[x])/euv + (Log[x]^2)/2 - (Log[1-x]^2)/2 + Li2[1] - 2 Li2[1-x] - Log[x]Log[1-x],
	K0 -> - Log[x]/((1-x) euv),
	P0 -> (I0 + Log[x])/euv - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
	P1 -> 1/euv + 2,
	
	P3 -> (I0 + Log[x] - 2)/euv - I1 + I0 Log[x] + (Log[x]^2)/2 - 4 + Li2[1],
	
	R0 -> 1/eir^2 - Li2[1],
	R1 -> 1/eir^2 + 2/eir + 4 - Li2[1],
	R2 -> -1/eir - 2,
	R3 -> 1/eir^2 + 3/eir + 7 - Li2[1],
	R4 -> -1/(2 eir) - 1,
	R5 -> -1/(2 eir) - 3/2,
	R6 -> 1/(4 euv) + 3/4,
	S0 -> 1/eir^2 - (I0 - Log[x])/eir + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x] - (Log[x]^2)/2,
	S1 -> 1/eir^2 - x Log[x]/((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
	S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x),

	S3 -> - ((I0 + Log[x]/(1-x))/eir - I1 + I0 Log[x]/(1-x) - Li2[1] - x Li2[1-x]/(1-x) + (Log[x]^2)/2),

	T0 -> 1/euv + 2,
	T1 -> 1/(2 euv) + 1,
	
	V1 -> (x Log[x] + 1-x)/(1-x)^2 / euv,
	V2 -> - (Log[x] + 1-x)/(1-x)^2 / euv,
	
	U0 -> 1/eir^2 + (Log[x] - 2 Log[1-x] - I0) / eir + I1 - I0 Log[x] + 2 Li2[1-x] - (Log[x]^2)/2 + Log[1-x]^2 - 6 Li2[1]
}


CollectLoopIntegrals[expr_, l_] := Module[
	{result, collectRules, signCorrectionRules, simplifyRules},
	
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
		
		$$[{},{},{}] -> 1
	};
	
	signCorrectionRules = {
		$$[{a___}, {b___}, {c___}] :>
			$$[{a}, -# &/@ {b}, -# &/@ {c}] /;
				{b,c} == Select[{b,c}, Or[# == 0, MatchQ[#, Times[-1, _Symbol]]] &]
	};
	
	simplifyRules = {
		$$[{a1___,x_,a2___},{0,b1___,x_,b2___},{c___}] :> 1/2 (
			$$[{a1,a2}, {0,b1,b2}, {c}]
			- $$[{a1,a2}, {b1,x,b2}, {c}] +
			- $$[{a1,a2}, {0,b1,x,b2}, {c}] x.x
		)
	};
	
	result = Expand[expr $$[{},{},{}]]
		//. collectRules
		/. signCorrectionRules;
	result = result
		/. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]];
	result = result /. simplifyRules;
	
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
	{collected, expanded, expansionRules, integrated, integrationRules,
		 phaseSpaceRule, result, simplified, unevaluated},
	
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
		$$[{x_, y_},{0,k,p},{}] :> - Q (k.k)^(-1-eir) (
    		p.x p.y R3 + k.x k.y R4 + (k.x p.y + p.x k.y) R5 + k.k x.y R6
   		),
   

		$$[{},{0,k},{0}] -> - Q (k.k)^(-eir) P0 / k.n,
		$$[{},{0,p},{0}] -> - Q (p.p)^(-eir) B0 / p.n,
		$$[{},{k,p},{0}] -> - Q (q.q)^(-eir) K0 / p.n,
		
		$$[{x_},{0,k},{0}] :> Q (k.k)^(-eir)/k.n (
			k.x P1 + n.x k.k/(2 k.n) P3
		),
		$$[{x_},{0,p},{0}] :> Q (p.p)^(-eir)/p.n (
			p.x B1 + n.x p.p/(2 p.n) B3
		),
		$$[{x_},{k,p},{0}] :> Q (q.q)^(-eir) (
			p.x V1 + k.x V2
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

		V1 -> (x Log[x] + 1-x)/(1-x)^2 / euv,
		V2 -> - (Log[x] + 1-x)/(1-x)^2 / euv
	};
	
	collected = CollectLoopIntegrals[expr, l];
	result["collected"] = collected;
(*
	simplified = SimplifyLoopIntegrals[expr, l];
	result["simplified"] = simplified;
*)
	integrated = Expand[
		collected
		/. integrationRules
		/. phaseSpaceRule
		/. $kinematicRules
	];
	result["integrated"] = integrated;
	unevaluated = Union[Cases[integrated, $$[__], Infinity]];
	If[
		unintegrated != {}
		,
		Message[
			IntegrateLoop::unevaluated,
			StringDrop[ToString[#], 16] &/@ unevaluated
		];
		result["expanded"] = Null;
		Return[result]
	];
	
	expanded = Expand[
		integrated
		/. expansionRules
	];
	result["expanded"] = expanded;
	
	result
];


ExtractPole[kernel_, eta_] := Expand[
	Coefficient[Series[kernel, {eta, 0, 1}], eta, -1]
];


(*---------------------------------------------------------------------------*)
(*--------------------------- SPLITTING FUNCTION ----------------------------*)
(*---------------------------------------------------------------------------*)

SplittingFunction[topology_] := Module[
	{counterterm, exclusive(*, kinematicRules*), result, trace},
	
	trace = Expand[
		GammaTrace[topology, NumberOfDimensions -> 4 + 2 eps]
			/. $kinematicRules
	];
	result["trace"] = trace;

	exclusive = IntegrateLoop[trace, l];
	result["exclusive"] = exclusive;
	If[
		exclusive["expanded"] == Null
		,
		Return[result]
	];

	counterterm = ExtractPole[
		exclusive["expanded"]
			/. {eps -> 0}
			/. $onShellRules
			/. {0^(-eir) -> 1, 0^(n_Integer-eir) :> 0 /; n>0}
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
		,
		euv
	];
	result["counterterm"] = counterterm; 

	result
];


PartonDensity[topology_, LO_:0] := Module[
	{ExclusiveLO, ExclusiveNLO, InclusiveNLO, KernelNLO, Z},
	
	KernelNLO = Expand[
		ContractIndices[
			GammaTrace[topology, NumberOfDimensions -> 4 + 2 eps]
				//. {KinematicRules}
		]
	];
	
	TT = Expand[
		Simplify[KernelNLO //. {IntegrateLoopRules}]
	] //. {ScalarProductRules};
	Debug["T", Collect[
		Expand[TT / (Gamma[1+eir] g^4 Pi^(-2+eir) 2^(2 eir))] //.
		    {OnShellRules, {0^(2-eir)->0, 0^(1-eir)->0}},
		{B0, B1, K0, P0, P1, P3, R0, R1, R2, R3, R4, R5, R6, S0, S1, S2, S3, T0, T1},
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
	(*
	Debug["DoublePole", Simplify[
		Coefficient[
			Series[ExclusiveNLO //. {eps -> -eir}, {eir, 0, 1}],
			eir,
			-2
		] //. {OnShellRules}
	]];
	*)
	
	v0 = Expand[
		ExclusiveNLO - 2^(2 eir) Pi^(eir) Gamma[1+eir] Z ExclusiveLO / euv
	];
	(*Debug["v0", v0];*)
	v1 = Expand[
		v0 //. {eir -> -eps, euv -> -eps}
	];
	(*Debug["v1", v1];*)
	v2 = Expand[v1 //. {OnShellRules, {0^(eps)->0, 0^(1+eps)->0, 0^(2+eps)->0}}];
	(*Debug["v2", v2];*)
	v3 = Expand[IntegrateFinal[v2, 4 + 2 eps]];
	(*Debug["v3", v3];*)
	v4 = ExtractPole[v3, eps];
	(*Debug["v4", v4];*)

	InclusiveNLO = Collect[
		Expand[Simplify[v4]],
		{
			(Log[x])^2, Log[x] Log[1 - x], x Log[x], I0 Log[x], I0 Log[1 - x],
			Log[x], Log[1 - x], I0, I1, Li2[1], Li2[1-x]
		},
		Simplify];
	
	{
		{"kernel", KernelNLO},
		{"exclusive", ExclusiveNLO},
		{"inclusive", InclusiveNLO},
		{"Z", Z}
	}
];


Print["Exiting AXILOOP..."];

End[]


EndPackage[]
