(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2013 Oleksandr Gituliar.                                    *)
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

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l] ** FV[i2] **
	GP[i1, i3, l+k] ** GP[i2, i4, l+p] ** GV[i3, k+l, i4, -l-p, mu, p-k] **
	FPx[p] ** GPx[mu, nu, p-k] ** FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];


Test[
	ExpandLoopIntegrals[$Get[$result, {"integrated", "collected"}], l]
	,
	$Get[$result, "trace"]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20130207-H1E3O8"
];

Test[
	$Get[$result, "Z"]
	,
	- I g^2 (4 Pi)^-2 (3 - 8 I0 - 4 Log[1-x] - 2 Log[x])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20130227-M1X6E0"
];


Test[
	$Get[$result, "exclusive-bare-short"]
	,
	Expand[
	- I Gamma[1+eir] g^4 2^(-3+2 eir) Pi^(-2+eir) / (k.k (-1+x)) (
	  (k.k)^(-eir) (
	      R0 (-1 + eps (-1 + x))
	    - (P1 (-1 + eps (-1 + x)) (-2 + x))
	    - (2 (1 + eps) R4 (-3 + x) x)
	    - (2 (1 + eps) R5 (-3 + x) x)
	    - (R1 x (eps (-1 + x) + x))
	    + (E1 x^2 (eps (-1 + x) + x))
	    + (2 E2 x^2 (eps (-1 + x) + x))
	    + (E3 x^3 (eps (-1 + x) + x))
	    + (R2 x (-1 + eps (-5 + x) + 2 x))
	    + (3 P0 (1 + eps (-1 + x)^2 + x^2))
	    - (U0 (1 + eps (-1 + x)^2 + x^2))
	    - (4 (1 + eps) R6 (1 - 4 x + x^2)))
	 + (q.q)^-eir (
	   3 C0 (1 + eps (-1 + x)^2 + x^2) + x (T0 (-3 + x) - 2 C1 (1 + x)))
	 + (p.p)^-eir (
	   -B1 x (eps (-1 + x) + x) + 2 B0 (1 + eps (-1 + x)^2 + x^2) + D0 (1 + eps (-1 + x)^2 + x^2))
	)
	]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20130603-F7S6X9"
];