(* Unit Tests for Axiloop Package
 *
 * Author:   Oleksandr Gituliar <gituliar@gmail.com>
 * Created:  04 May 2012
 *
 * Copyright (c) 2012 Oleksandr Gituliar
 *)

Needs["Axiloop`"]

(* EquivalenceFunction for Test. It turns out that Equal doesn't work with expressions. *)
EqualSimplify[x_, y_] := SameQ[True, Equal[Simplify[x], Simplify[y]]];

Test[
	Axiloop`Tracer`TracerVersion,
	"1.1.1",
	TestID->"TracerVersion"
]

Test[
	Axiloop`Tracer`Private`d,
	4-2*epsilon,
	TestID->"VectorDimension"
]

Test[
	FP[k, f1],
	I * G[f1, k] / k.k,
	TestID->"FermionPropagator"
]

Test[
	FV[mu, f1],
	- I g G[f1, {mu}],
	TestID->"FermionVertex"
]

Test[
	GP[mu, mu, p],
	- I 2 (1 - epsilon) / p.p,
	TestID->"GluonPropagator",
	EquivalenceFunction->EqualSimplify
]

Test[
	GV[mu,p, mu,k, nu,p],
	g (-3 + 2 epsilon) (k.{nu} - p.{nu}),
	TestID->"GluonVertex",
	EquivalenceFunction->EqualSimplify
]
