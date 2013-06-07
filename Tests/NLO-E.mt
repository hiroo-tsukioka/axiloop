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


Get["Tests/core.mt"];


ExpandLoopIntegrals = Axiloop`Integrate`Private`$$ExpandLoopIntegrals;

$LO = << "LO.result";

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[k - l] ** GP[i1, i2, l] **
  FV[i2] ** FP[k] ** FV[mu] ** FPx[p] ** GPx[mu, nu, p - k] ** 
  FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];


Test[
	ExpandLoopIntegrals[$Get[$result, {"integrated", "collected"}], l]
	,
	$Get[$result, "trace"]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-E-20130211-Y4Q8U2"
];

(*
Test[
	ExpandLoopIntegrals[$Get[$result, {"integrated", "simplified"}], l]
	,
	$Get[$result, "trace"]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-E-20130211-N4R8P0"
];
*)

Test[
	$Get[$result, "Z"]
	,
	g^2 (4 Pi)^-2 (3 - 4 I0 - 4 Log[x])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-E-20130211-R1R7V3"
];

Test[
	$Get[$result, "inclusive"]
	,
	(g/(4 Pi))^4 ( (1+x^2)/(1-x) (7 - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] - 4 Li2[1] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0) )
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-E-20130211-J4U0Y0"
]



$$factors = ExtractFormFactors[$Get[$result, "exclusive-bare"]];

$$k$0  = $Get[$$factors, "$$k$0"];
$$k$ir = $Get[$$factors, "$$k$ir"];
$$k$uv = $Get[$$factors, "$$k$uv"];
$$p$uv = $Get[$$factors, "$$p$uv"];
$$q$uv = $Get[$$factors, "$$q$uv"];

Test[
	Expand[$Get[$result, "inclusive"]]
	,
	Expand[I (g/(4 Pi))^4 (($$k$uv + $$p$uv + $$q$uv) (Log[1-x]/2 + (1-x)^2/(1+x^2)) + $$k$0/2)]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-E-20130424-K4J3R3"
]