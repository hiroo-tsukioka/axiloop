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

n::usage =
	"Light-cone gauge vector."

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


Begin["`Private`"]

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

FP[p_, line_] := FPx[p, line] / p.p;

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

End[]


EndPackage[]
