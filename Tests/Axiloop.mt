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
	FP[k, f1],
	I * G[k, f1] / k.k,
	TestID->"FermionPropagator"
]

Test[
	FV[mu, f1],
	- I g G[{mu}, f1],
	TestID->"FermionVertex"
]

Test[
	GP[mu, mu, p],
	I (2 - d) / p.p,
	TestID->"GluonPropagator",
	EquivalenceFunction->EqualSimplify
]

Test[
	Simplify[GV[mu,p, mu,k, nu,p]],
	g (1 - d) (k.{nu} - p.{nu}),
	TestID->"GluonVertex",
	EquivalenceFunction->EqualSimplify
]

LO = x/(4 k.n) Kernel[G[n]**FP[k]**FV[mu]**FPx[p]**GPx[mu,nu,p-k]**FV[nu]**FP[k]];

Test[
	LO,
	(1/((k.k)^2 k.n (k.n - n.p)))
	* 2 g^2 x (k.k ((-1 - eps) (k.n)^2 
    + 2 eps k.n n.p + (-1 - eps) (n.p)^2)
    + k.n (-(-1 - eps) n.p (p.p - q.q)
    + k.n ((1 - eps) p.p - (-1 - eps) q.q))),
	TestID->"LO-Kernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[1/k.k],
	1/Gamma[1+eps] (4 Pi)^(-2-eps) (1-x)^eps (k.k)^eps / eps,
	TestID->"IntegrateKernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[LO] //. {p.p -> 0, q.q -> 0},
	((2^(-3 - 2*eps)*g^2*Pi^(-2 - eps)*x*((1 + eps)*S[k, n]^2 - 2*eps*S[k, n]*S[n, p] + (1 + eps)*S[n, p]^2))/(-eps*(1 - x)^(-eps)*Gamma[1 + eps]*S[k, k]^(-eps)*S[k, n]*(S[k, n] - S[n, p]))),
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
	TestID->"ReduceIntegral-3",
	EquivalenceFunction->EqualSimplify
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

Test[
	GetValue[0, "key", 1],
	1,
	TestID->"GetValue-1"
]

Test[
	GetValue[{{"key1", 0}, {"key2", 2}}, "key2", 1],
	2,
	TestID->"GetValue-2"
]

Test[
	GetValue[{{"key1", 0}, {"key2", 2}}, "key3", 3],
	3,
	TestID->"GetValue-3"
]


Test[
	S[-a,b],
	- a.b,
	TestID->"ScalarProduct-01"
]


Test[
    Counterterm[
        {{"exclusive", {{"expanded", (1 - x)/eta + x}}}},
        {{"exclusive", {{"expanded", 1}}}},
        eta
    ],
    1-x,
    TestID->"Counterterm"
 ]