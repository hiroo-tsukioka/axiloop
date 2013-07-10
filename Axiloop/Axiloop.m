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


BeginPackage["Axiloop`", {
	"Axiloop`Core`",
	"Axiloop`Integrate`",
	"Axiloop`Tracer`"
}]

Clear[ "Axiloop`*" , "Axiloop`Private`*"];

Axiloop`$Author = "Oleksandr Gituliar <oleksandr@gituliar.org>";
Axiloop`$Version = "1.6a (June 2013)";

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

ExtractFormFactors::usage = ""

IntegrateFinal::usage =
	"Integrate over final-state momenta."

SplittingFunction::usage = ""


Begin["`Private`"]


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)


(* Useful modifications to standard functions *)

Unprotect[Dot];
    (-x_).y_ := -x.y;
Protect[Dot];

(*
Unprotect[S];
    S[n,n] = 0;
Protect[S];
*)

CollectExclusiveShort[expr_] := Module[{},
	Collect[
		expr /. {eps -> 0, (k.k)^(-1-eir) -> (k.k)^-1, (p.p)^(-eir) -> 1, (q.q)^(-eir) -> 1}
		(*
			/. {eps->0, (k.k)^(n_Integer-eir):>(k.k)^n, p.p->0, q.q->0}
			/. {0^(-eir)->1, 0^(1-eir)->0, 0^(2-eir)->0}
		*)
		,
		{B0,B1,B3,C0,C1,D0,K0,P0,P1,P3,R0,R1,R2,R3,R4,R5,R6,S0,S1,S2,T0,T1,V0,V1,V2,U0}
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
	(4 Pi)^(-2+eps)/Gamma[1-eps] (1 + eps Log[1-x])	Integrate[Expand[(k.k)^(eps) kernel], k.k]
];


(*---------------------------------------------------------------------------*)
(*------------------------ LOOP MOMENTA INTEGRATION -------------------------*)
(*---------------------------------------------------------------------------*)

$onShellRules = {
	p.p -> 0,
	q.q -> 0
}


(*---------------------------------------------------------------------------*)
(*--------------------------- SPLITTING FUNCTION ----------------------------*)
(*---------------------------------------------------------------------------*)

Options[SplittingFunction] = {IntegrateLoopPrescription -> "MPV"};
SplittingFunction[$topology_, $LO_:Null, OptionsPattern[]] := Module[
	{counterterm, exclusive, exclusiveBare, exclusiveBareShort, inclusive,
	 integrated, trace, Z, $PutOnShell},
	
	$PutOnShell[expr_] := Replace[
		Expand[expr] /. {
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
		GammaTrace[Expand[$topology], NumberOfDimensions -> 4 + 2 eps]
			/. $kinematicRules
	];

	integrated = IntegrateLoop[
		trace,
		l,
		Prescription -> OptionValue[IntegrateLoopPrescription]
	];
	
	exclusiveBareShort = If[
		SameQ[$LO, Null]
		,
		Null
		,
		$PutOnShell[$Get[integrated, {"integrated", "short"}]]
	] /. {k.n -> x, n.p -> 1, n.q -> 1-x};
	
	exclusiveBare = $PutOnShell[$Get[integrated, {"integrated", "long"}]]
		/. {eps^2 -> 0}
		/. {k.n -> x, n.p -> 1, n.q -> 1-x};
	
	Z = Simplify[If[
		SameQ[$LO, Null]
		,
		0
		,
		PolePart[
			exclusiveBare
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
	
	exclusive = exclusiveBare - counterterm / euv
		/. {eir -> - eps, euv -> -eps}
		/. {p.p -> 0, q.q -> 0}
		/. {0^eps -> 0, 0^(1+eps) -> 0}
	;

	inclusive = Expand[
		PolePart[
			IntegrateFinal[Expand[exclusive], 4 + 2 eps]
			,
			eps
		]
	];
	
	{
		{"trace", trace},
		{"integrated", integrated},
		{"Z", Z},
		{"counterterm", counterterm},
		{"exclusive-bare", exclusiveBare},
		{"exclusive-bare-short", exclusiveBareShort},
		{"exclusive", exclusive},
		{"inclusive", inclusive}
	}
];


ExtractFormFactors[bare_] := Module[
	{$$k$uv, $$p$uv, $$q$uv, $$k$ir, $$k$ir2, $$k$0, $$bare},
	
	$$bare = Expand[
		bare / (I g^4 (4 Pi)^(-2+eir) Gamma[1+eir] / k.k)
	];
	
	$$k$ir2 = PolePart[
		$$bare
			/. {eps -> -eir}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		eir
		,
		-2
	] /. {p.p -> 0, q.q -> 0};
	DEBUG[
		"ExtractFormFactors::$$k$ir2"
		,
		Simplify[$$k$ir2]
	];
	
	$$k$ir = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -eir}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		eir
	];
	DEBUG[
		"ExtractFormFactors::$$k$ir"
		,
		$$k$ir
	];
	
	$$k$uv = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -euv}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		euv
	];
	DEBUG[
		"SplittingFunction::$$k$uv"
		,
		$$k$uv
	];

	$$p$uv = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 0, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 1, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -euv}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		euv
	];
	DEBUG[
		"SplittingFunction::$$p$uv"
		,
		$$p$uv
	];


	$$q$uv = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 0, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 1, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -euv}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		euv
	];
	DEBUG[
		"SplittingFunction::$$q$uv"
		,
		$$q$uv
	];
	
	$$k$0 = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eir -> -eps, euv -> -eps}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		eps
		,
		0
	];
	DEBUG[
		"SplittingFunction::$$k$0"
		,
		Collect[
			$$k$0
			,
			{I0, Log[x], Log[1-x], I1, Li2[1], Li2[1-x]}
			,
			Simplify
		]
	];
	
	
	DEBUG[
		"SplittingFunction:: $$k$uv + $$p$uv + $$q$uv"
		,
		Collect[
			Expand[($$k$uv + $$p$uv + $$q$uv)]
			,
			{I0, Log[x], Log[1-x]}
			,
			Simplify
		]
	];
	
	DEBUG[
		"SplittingFunction:: $$p$uv + $$q$uv - $$k$ir"
		,
		Collect[
			Expand[$$p$uv + $$q$uv - $$k$ir]
			,
			{I0, Log[x], Log[1-x]}
			,
			Simplify
		]
	];

    {
        {"$$k$0", $$k$0},
        {"$$k$ir", $$k$ir},
        {"$$k$ir2", $$k$ir2},
        {"$$k$uv", $$k$uv},
        {"$$p$uv", $$p$uv},
        {"$$q$uv", $$q$uv}
    }
];

End[]


EndPackage[]
