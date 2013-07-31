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

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l+k] ** FV[mu] **
	FP[l+p] ** FV[i2] ** GP[i1, i2, l] ** FPx[p] ** GPx[mu, nu, p-k] **
	FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];

(*
Test[
	ExpandLoopIntegrals[$Get[$result, {"integrated", "collected"}], l]
	,
	$Get[$result, "trace"]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130121-R0Q3G6"
];
*)

Test[
	$Get[$result, "Z"]
	,
	g^2 (4 Pi)^-2 (-3 + 4 I0 + 2 Log[x])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130123-M1X6E0"
];


Test[
	Simplify[
		$Get[$result, "inclusive"]
		-
		(g/(4 Pi))^4 ((1+x^2)/(1-x) (-7 + 2 Log[x]^2 + 2 Log[x] Log[1-x] - 3 Log[1-x] + 2 Li2[1-x] + 4 Li2[1] - 4 I1 + 4 I0 Log[x] + 4 I0 Log[1-x]) - (1-x) (3 - 2 Log[x] - 4 I0) + x)
	]
	,
	0
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130129-V7Y1C7"
];


$real = g^4 / Pi^4 (
			(1+x^2)/(1-x) (
				(Log[x])^2 + 2 Li2[1-x]
			)
			+
			7 (1-x) + 2(1+x)Log[x] + 1 + 3/(1-x)Log[x]
		);

$virt = Expand[Simplify[-256 $Get[$result, "inclusive"]]];

$full = g^4 / Pi^4 (
			(1+x^2)/(1-x) (
				7 - 4 Li2[1] - (Log[x])^2 - 2 Log[x] Log[1-x] + 3/2 Log[x] + 3 Log[1-x] + 4 I1 - 4 I0 (Log[x] + Log[1-x])
			)
			+ 7/2 (1+x) Log[x] + (1-x) (11 - 2 Log[x] - 4 I0)
		);

Test[
	Simplify[Expand[$virt + $real - $full]]
	,
	0
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130607-U9L9G5"
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
	TestID -> "NLO-C-20130424-K4J3R3"
]


Test[
	$$k$0
	,
	4 I ((4 + 5 x + 5 x^2)/(2(1-x)) + (Log[x] + 2 I0) (1-x) + (2 I1 - 2 I0 Log[x] - Log[x]^2 - Li2[1-x] - 2 Li2[1]) (1+x^2)/(1-x))
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130731-Q3N4H5"
]

Test[
	$$k$ir
	,
	4 I ((1-x)^3 + (1+x^3) Log[x])/(1-x)^2
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130731-U3Q1X0"
]

Test[
	$$k$uv
	,
	4 I ((1-x) (1/2 + 2 x + x^2/2 - 2 I0 (1 + x^2)) - (2 - x + x^2) Log[x])/(1-x)^2
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130731-C0U9T5"
]

Test[
	$$p$uv
	,
	0
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130731-D2X8V5"
]

Test[
	$$q$uv
	,
	4 I ((1-x)^3 + (1 + x^3) Log[x])/(1-x)^2
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-C-20130731-X9T8Y2"
]