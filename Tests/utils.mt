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

(* Some routines commonly used within test cases.                             *)

(* This function is supposed to be passed as `EquivalenceFunction` to `Test`. *)
EquivalentQ[x_, y_] := Module[{},
	If[ListQ[x] && ListQ[y],
		EquivalentQ[First[x], First[y]] && EquivalentQ[Rest[x], Rest[y]],
	If[StringQ[x] && StringQ[y],
		TrueQ[x == y],
		TrueQ[Simplify[x-y == 0]]
	]]
];
