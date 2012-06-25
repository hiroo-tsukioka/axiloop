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

(*  Unit tests for Axiloop package.                                           *)

Get["Tests/main.mt"];

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

LOdefinition = DefineKernel[FP[k] ** FV[mu], {FPx[p], GPx[mu, nu, q]}, FV[nu] ** FP[k]];

Test[
	LOdefinition,
	2 g^2 (k.k (1 - epsilon) - x (p.p + k.k) (1 - epsilon) + 2 x (k.k - x p.p)/(1 - x)) / (k.k)^2,
	TestID->"LO-Kernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[1/k.k],
	1/Gamma[1-epsilon] (4 Pi)^(-2+epsilon) (1-x)^(-epsilon) (k.k)^(-epsilon) / (-epsilon),
	TestID->"IntegrateKernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[LOdefinition] //. p.p -> 0,
	1/Gamma[1-epsilon] 2 g^2 (4 Pi)^(-2+epsilon) (1-x)^(-1-epsilon) (1 + x^2 - epsilon (1-x)^2) (k.k)^(-epsilon) / (-epsilon),
	TestID->"IntegrateLOKernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`CollectIntegral[l.k1 l.k2 / (l.l (l-p1).(l-p1) (l-p2).(l-p2)) 1/(l.n (l-q1).n (l-q2).n), l],
	Axiloop`Private`KK[l, {k2,k1}, {p2,p1,0}, {q2,q1,0}],
	TestID->"CollectIntegral-1",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`CollectIntegral[x, l],
	x,
	TestID->"CollectIntegral-2",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`ReduceIntegral[Axiloop`Private`KK[l, {l}, {0}, {k1}], l],
	Axiloop`Private`KK[l, {},{},{0}],
	TestID->"ReduceIntegral-1"
]

Test[
	Axiloop`Private`ReduceIntegral[Axiloop`Private`KK[l, {k1}, {0}, {k1}], l],
	Axiloop`Private`KK[l, {k1}, {-k1}, {0}] + Axiloop`Private`KK[l, {}, {-k1}, {0}] k1.k1,
	TestID->"ReduceIntegral-2"
]

Test[
	Axiloop`Private`ReduceIntegral[Axiloop`Private`KK[l, {}, {0}, {k1, k2}], l],
	(Axiloop`Private`KK[l, {}, {-k1}, {0}] - Axiloop`Private`KK[l, {}, {-k2}, {0}]) / (k1.n - k2.n),
	TestID->"ReduceIntegral-3"
]


Test[
	ExtractPole[Beta[1+eta, eta], eta],
	1,
	TestID->"ExtractPole-1"
]

Test[
	ExtractPole[Beta[1+eta, 1+eta] / eta^2, eta],
	-2,
	TestID->"ExtractPole-2"
]


LO = Kernel[FP[k] ** FV[mu], {FPx[p], GPx[mu, nu, p - k]}, FV[nu] ** FP[k]];
 
Test[
	GetValue[LO, "exclusive"],
	{
		{"compact", 2 g^2 ((1-x)(1-epsilon) +  2 x / (1-x)) / k.k},
		{"expanded", 2 g^2 ((1-x) (1-epsilon) + 2 x / (1-x)) / k.k}
	},
	TestID->"Kernel LO exclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[LO, "inclusive"],
	- g^2 (1 + x^2) / (8 Pi^2 (1 - x) ),
	TestID->"Kernel LO inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[LO, "Z"],
	0,
	TestID->"Kernel LO Z"
]


NLOc = Kernel[
	FP[k] ** FV[i1] ** FP[l - k] ** FV[mu] ** FP[l - p] ** FV[i2] ** GP[i1, i2, l],
	{ FPx[p], GPx[mu, nu, p - k]},
	FV[nu] ** FP[k],
	LO
]

Test[
	GetValue[NLOc, "inclusive"],
	- (g/(4 Pi))^4 ( (1+x^2)/(1-x) (-7 + 2 Log[x]^2 + 2 Log[x] Log[1-x] - 3 Log[1-x] + 2 Li2[1-x] + 4 Li2[1] - 4 I1 + 4 I0 Log[x] + 4 I0 Log[1-x]) - (1-x) (3 - 2 Log[x] - 4 I0) + x ),
	TestID->"Kernel NLOc inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOc, "Z"],
	- (g/(4 Pi))^2 (3 - 4 I0 - 2 Log[x]),
	TestID->"Kernel NLOc Z",
	EquivalenceFunction->EqualSimplify
]


NLOd = Kernel[
	FP[k] ** FV[i1] ** FP[l] ** FV[i2] ** GP[i1, i3, l-k] ** GP[i2, i4, l-p] ** GV[i3,k-l, i4,l-p, mu,p-k],
	{ FPx[p], GPx[mu, nu, p-k]},
	FV[nu] ** FP[k],
	LO
]

Test[
	GetValue[NLOd, "Z"],
	- (g/(4 Pi))^2 (3/2 - 4 I0 - Log[x] - Log[1-x]),
	TestID->"Kernel NLOd Z",
	EquivalenceFunction->EqualSimplify
]


NLOe = Kernel[
	FP[k] ** FV[i1] ** FP[k-l] ** GP[i1, i2, l] ** FV[i2] ** FP[k] ** FV[mu],
	{FPx[p], GPx[mu, nu, p-k]},
	FV[nu] ** FP[k],
	LO
]

Test[
	GetValue[NLOe, "inclusive"],
	- (g/(4 Pi))^4 ( (1+x^2)/(1-x) (7 - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] - 4 Li2[1] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0) ),
	TestID->"Kernel NLOe inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOe, "Z"],
	(g/(4 Pi))^2 (3 - 4 I0 - 4 Log[x]),
	TestID->"Kernel NLOe Z",
	EquivalenceFunction->EqualSimplify
]
