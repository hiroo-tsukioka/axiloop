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