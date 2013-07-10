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

DEBUG::usage = "";
INFO::usage = "";
WARN::usage = "";

$UnevaluatedError;


$Get::usage =
	"$Get[hash_, key_] get value by key from a hash table."

PolePart::usage =
	"PolePart[expr, x] extract coefficient in front of 1/x in expr."

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

$$debug = True;


Begin["`Private`"]

$$Message[level_, label_, message_] := Module[{},
	Print[level, "::", label, " : ", message];
];

DEBUG[label_, message_] := Module[{},
	If[
		$$debug
		,
		$$Message["DEBUG", label, message]
	]
];

INFO[label_, message_] := Module[{},
	$$Message["INFO", label, message]
];

WARN[label_, message_] := Module[{},
	$$Message["WARNING ", label, message]
];


$kinematicRules = {
	k.p -> (p.p + k.k - q.q) / 2,
	k.q -> (p.p - k.k - q.q) / 2,
	p.q -> (p.p - k.k + q.q) / 2,

	n.n -> 0
};

PolePart[kernel_, eta_, n_:-1] := Expand[
	Coefficient[Series[kernel, {eta, 0, n+1}], eta, n]
];


$Get[hash_, keys_, default_:Null] := Module[
	{item, key, value},
	
	key = If[
		ListQ[keys]
		,
		First[keys]
		,
		keys
	];

	item = Select[hash, First[#] == key &, 1];

	value = If[
		item == {}
		,
		default
		,
		Last[First[item]]
	];
	
	If[
		!ListQ[keys] || Length[keys] == 1
		,
		value
		,
		$Get[value, Rest[keys]]
	]
];

End[]


EndPackage[]
