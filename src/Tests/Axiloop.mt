(* Unit Tests for Axiloop Package
 *
 * Author:   Oleksandr Gituliar <gituliar@gmail.com>
 * Created:  04 May 2012
 *
 * Copyright (c) 2012 Oleksandr Gituliar
 *)

Needs["Axiloop`"]

(* EquivalenceFunction for Test. It turns out that Equal doesn't do the job. *)
EqualSimplify[x_, y_] := SameQ[0, Simplify[x-y]];

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

LOKernel = Kernel[FP[k] ** FV[mu], {FPx[p], GPx[mu, nu, q]}, FV[nu] ** FP[k]];

Test[
	LOKernel,
	2 g^2 (k.k (1 - epsilon) - x (p.p + k.k) (1 - epsilon) + 2 x (k.k - x p.p)/(1 - x)) / (k.k)^2,
	TestID->"LO-Kernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[1/k.k],
	1/(4 Pi)^2 (1-x)^(-epsilon) (k.k)^(-epsilon) / (-epsilon),
	TestID->"IntegrateKernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[LOKernel],
	2 g^2/(4 Pi)^2 (1-x)^(-1-epsilon) (1 + x^2 - epsilon (1-x)^2) (k.k)^(-epsilon) / (-epsilon),
	TestID->"IntegrateLOKernel",
	EquivalenceFunction->EqualSimplify
]
