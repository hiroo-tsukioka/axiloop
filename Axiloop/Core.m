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


BeginPackage["Axiloop`Core`"]

DebugInfo::usage = ""

$kinematicRules::usage = ""

eps::usage =
	"Dimensional regulator; n = 4 + 2 eps."

g::usage =
	"Quark-gluon coupling constant."

x::usage =
	"x = k.n/p.n"

k::usage =
	"Outgoing particle momentum; k.n = x."

p::usage =
	"Incoming particle momentum; p.n = 1."

q::usage = "Final state particle momentum; q.q = 0."

n::usage =
	"Light-cone gauge vector; n.n = 0."


Begin["`Private`"]

$debug = True;

$kinematicRules = {
	k.p -> (p.p + k.k - q.q) / 2,
	k.q -> (p.p - k.k - q.q) / 2,
	p.q -> (p.p - k.k + q.q) / 2,

	n.n -> 0
};

DebugInfo[sender_, message_] := If[
	$debug
	,
	Print[
		"DebugInfo::"
		,
		sender
		,
		" : "
		,
		message
	]
];

End[]

EndPackage[]