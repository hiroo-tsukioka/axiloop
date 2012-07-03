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


NLOe = Kernel[
	FP[k] ** FV[i1] ** FP[k-l] ** GP[i1, i2, l] ** FV[i2] ** FP[k] ** FV[mu],
	{FPx[p], GPx[mu, nu, p-k]},
	FV[nu] ** FP[k],
	LO
]

Test[
	GetValue[GetValue[NLOe, "exclusive"], "compact"],
	2 g^4 (4 Pi)^(-2+eta) Gamma[1+eta] (k.k)^(-1-eta) / (1-x) (
		  Axiloop`Private`P0 (-4) (1+x^2 - (1-x)^2 epsilon)
		+ Axiloop`Private`P1 (1-epsilon(1-x))(2-x)
		+ Axiloop`Private`T0 (1+x+4x^2 + epsilon(4-3x)x - epsilon^2 (1-x))
		+ Axiloop`Private`T1 (-x^2 + epsilon(-x+2x^2) + epsilon^2 (x-x^2))
	),
	TestID->"Kernel NLOe exclusive compact",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[GetValue[NLOe, "exclusive"], "expanded"],
	2 g^4 (4 Pi)^(-2+eta) Gamma[1+eta] (k.k)^(-1-eta) / (eta (1-x))
	* (1+x^2 - epsilon(1-x)^2)
	* (
	    (3 + epsilon)Beta[1-eta,1-eta]
	    - 4 (I0 - eta I1 - eta Li2[1] + (1+eta)Log[x] + eta Log^2[x])
	),
	TestID->"Kernel NLOe exclusive expanded",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOe, "inclusive"],
	- (g/(4 Pi))^4 ( (1+x^2)/(1-x) (7 - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] - 4 Li2[1] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0) ),
	TestID->"Kernel NLOe inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOe, "Z"],
	(g/(4 Pi))^2 (3 - 4 I0 - 4 Log[x]),
	TestID->"Kernel NLOe Z",
	EquivalenceFunction->EqualSimplify
]