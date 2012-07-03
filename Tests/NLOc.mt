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


NLOc = Kernel[
	FP[k] ** FV[i1] ** FP[l - k] ** FV[mu] ** FP[l - p] ** FV[i2] ** GP[i1, i2, l],
	{ FPx[p], GPx[mu, nu, p - k]},
	FV[nu] ** FP[k],
	LO
]

Test[
	GetValue[GetValue[NLOc, "exclusive"], "compact"],
	2 g^4 (4 Pi)^(-2+eta) Gamma[1+eta] (k.k)^(-1-eta) / (1-x) (
		  Axiloop`Private`P0 3 ((1+x^2) - epsilon (1-x)^2)
		+ Axiloop`Private`P1 (-1 + epsilon(1-x))(2-x)
		+ Axiloop`Private`R0 (3+x^2 - epsilon (2-3x+x^2) - epsilon^2 (1-x))
		+ Axiloop`Private`R1 (-3 + (2-x) epsilon + (1-x) epsilon^2)
		+ Axiloop`Private`R2 x (-3 + (2-x) epsilon + (1-x) epsilon^2)
		+ Axiloop`Private`R3 (1-epsilon)
		+ Axiloop`Private`R4 (1-epsilon) x^2
		+ Axiloop`Private`R5 (1-epsilon) 2 x
		+ Axiloop`Private`S0 (-1-x^2 + epsilon (1-x)^2)
		+ Axiloop`Private`T0 (2(1-x-2x^2) - epsilon(2-x) - epsilon^2 (1-x)x)
		+ Axiloop`Private`T1 (1-epsilon) (3x-2)x/2
	),
	TestID->"Kernel NLOc exclusive compact",
	EquivalenceFunction->EqualSimplify
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