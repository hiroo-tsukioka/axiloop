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

Needs["Axiloop`"]

Get["Tests/core.mt"];


Test[
	GammaTrace[G[{mu}]**G[{mu}], NumberOfDimensions -> 4 + eps]
	,
	4 (4 + eps)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"GammaTrace-20130116-N5X7J3"
]

Test[
	GammaTrace[G[{mu}]**G[{nu}]**G[{mu}]**G[{nu}], NumberOfDimensions -> 4 - eps]
	,
	- 4 ((4 - eps))^2 + 8 (4 - eps)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"GammaTrace-20130116-T9V2C4"
]

Test[
	GammaTrace[G[{mu}]**G[{nu}], NumberOfDimensions -> 4 + eps]
	,
	4 {mu}.{nu}
	,
	TestID->"GammaTrace-20130116-G1O7O5"
]

Test[
	GammaTrace[
		G[{mu}]**G[{nu}]**G[{mu}]**G[{xi}],
		NumberOfDimensions -> 4 + eps
	]
	,
	- 4 (2 + eps) {nu}.{xi}
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"GammaTrace-20130116-Z8F5E3"
]

Test[
	GammaTrace[
		G[{mu}, Line->f2]**G[{nu}]**G[{mu}, Line->f2]**G[{xi}],
		NumberOfDimensions -> 4 + eps
	]
	,
	4 (4 + eps) 4 {nu}.{xi}
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"GammaTrace-20130116-U6U0O7"
]
