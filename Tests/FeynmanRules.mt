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


Get["Tests/main.mt"];


Test[
	GammaTrace[FP[p]**FP[k], NumberOfDimensions -> 4 + eps],
	- 4 k.p / (k.k p.p),
	TestID->"Test 01",
	EquivalenceFunction -> EqualSimplify
]


Test[
	GammaTrace[FP[p]**FV[{mu}]**FPx[k]**FV[{mu}], NumberOfDimensions -> 4 + eps],
	- 4  (2 + eps) g^2 k.p / p.p,
	TestID->"Test 02",
	EquivalenceFunction -> EqualSimplify
]