(* Axiloop Package for Wolfram Mathematica
 *
 * Author:   Oleksandr Gituliar <gituliar@gmail.com>
 * Created:  04 May 2012
 *
 * Copyright (c) 2012 Oleksandr Gituliar
 *)


BeginPackage["Axiloop`", {"Axiloop`Tracer`"}]

epsilon::usage =
	"Dimensional regulator; comes from trace operations."

g::usage =
	"Quark-gluon coupling constant."

k::usage =
	"Outgoing particle momentum; k.n = x."

n::usage =
	"Light-cone gauge vector; n.n = 0."

p::usage =
	"Incoming particle momentum; p.n = 1."

x::usage =
	"x = k.n"

FPx::usage =
	"Crossed fermion propagator in light-cone gauge."
	
FP::usage =
	"Fermion propagator in light-cone gauge."

FV::usage =
	"Fermion vertex in light-cone gauge."
	
GPx::usage =
	"Crossed gluon propagator in light-cone gauge."
	
GP::usage =
	"Gluon propagator in light-cone gauge."

GV::usage =
	"Gluon vertex in light-cone gauge."

Kernel::usage =
	"Defines new DGLAP evolution kernel."

Begin["`Private`"]

Unprotect[S];
  S[n,n] = 0;
  S[n,p] = 1;
  S[p,n] = 1;
  S[n,k] = x;
  S[k,n] = x;
Protect[S];

Spur[f0];

(* Set a space-time dimension where traces are calculated. *)

VectorDimension[4-2*epsilon];

(* Define Feynman rules for light-cone gauge. *)

fermionLines = {};

LorentzTensor[mu_, nu_, p_] :=
	- {mu}.{nu} + (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n;

FPx[p_, line_:f1] := (
	fermionLines = Union[fermionLines, {line}];
	I G[line, p]
);

FP[p_, line_:f1] := FPx[p, line] / p.p;

FV[mu_, line_:f1] := (
	fermionLines = Union[fermionLines, {line}];
	- I g G[line, {mu}]
);

GPx[mu_, nu_, p_] :=
	I LorentzTensor[mu, nu, p];

GP[mu_, nu_, p_] :=
	GPx[mu, nu, p] / p.p;

GV[i1_,p1_, i2_,p2_, i3_,p3_] :=
	g ( {i1}.{i2} (p1.{i3}-p2.{i3}) + {i2}.{i3} (p2.{i1}-p3.{i1}) + {i3}.{i1} (p3.{i2}-p1.{i2}) );

(* The definition of DGLAP evolution kernel according to CFP *)

Kernel[L_, M__, R_] := Module[{spurRules = (#->f0)& /@ fermionLines},
    x ExpandNumerator[(G[f1,n]/(4 k.n))**L**NonCommutativeMultiply@@M**R //. spurRules]
];

End[]


EndPackage[]
