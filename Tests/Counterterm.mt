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
	Counterterm[a/eta + b, 0, eta],
	0,
	TestID->"Test 01"
]

Test[
	Counterterm[a/eta + b, 1, eta],
	a,
	TestID->"Test 02"
]

Test[
	Counterterm[(1+x)(1+y)/eta, 1+x, eta],
	1+y,
	TestID->"Test 03"
]

Test[
	Counterterm[a/eta^2 + b/eta + c, 1, eta],
	b,
	TestID->"Test 04"
]
