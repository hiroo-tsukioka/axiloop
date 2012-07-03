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

Get["Tests/LO.mt"];


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