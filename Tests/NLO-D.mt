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
	GP[i1, i3, l-k] ** GP[i2, i4, l-p] ** GV[i3, k-l, i4, l-p, mu, p-k] **
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
	I g^2 (4 Pi)^-2 (3 - 8 I0 - 4 Log[1-x] - 2 Log[x])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20130227-M1X6E0"
];
