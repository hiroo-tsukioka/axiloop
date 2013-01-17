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

Needs["Axiloop`"]

Get["Tests/utils.mt"]


CollectLoopIntegrals = Axiloop`Private`CollectLoopIntegrals;
$$ = Axiloop`Private`$$;

Test[
	CollectLoopIntegrals[l.k l.p / l.n + X, l]
	,
	$$[{k,p},{},{0}] + X
	,
	TestID->"CollectLoopIntegrals-20130117-B1H3G4"
];

Test[
	CollectLoopIntegrals[(l.k)^2 l.p / l.n, l]
	,
	$$[{k,k,p},{},{0}]
	,
	TestID->"CollectLoopIntegrals-20130117-Z8O3Q1"
];

Test[
	CollectLoopIntegrals[(l.k)^2 (l.p)^3 / l.n, l]
	,
	$$[{k,k,p,p,p},{},{0}]
	,
	TestID->"CollectLoopIntegrals-20130117-O6U6V9"
];

Test[
	CollectLoopIntegrals[(l.k)^2 (l.p)^3 l.q / l.n, l]
	,
	$$[{k,k,p,p,p,q},{},{0}]
	,
	TestID->"CollectLoopIntegrals-20130117-W4Q2H7"
];

Test[
	CollectLoopIntegrals[l.k l.p / l.n + l.q l.p / (k.n l.n), l]
	,
	$$[{k,p},{},{0}] + $$[{p,q},{},{0}] / k.n
	,
	TestID->"CollectLoopIntegrals-20130117-H7N1A3"
];

Test[
	CollectLoopIntegrals[1 / ((l+a).(l+a) l.n), l]
	,
	$$[{},{a},{0}]
	,
	TestID->"CollectLoopIntegrals-20130117-B4I6E3"
];

Test[
	CollectLoopIntegrals[l.a / ((l-b).(l-b) (l+c).(l+c) l.n), l]
	,
	$$[{a},{-b,c},{0}]
	,
	TestID->"CollectLoopIntegrals-20130117-A1G0C3"
];

(*
TestCase[input_, output_, testid_] := Module[{},
	Test[
		Axiloop`Private`CollectIntegral[input, l],
		output,
		TestID->testid,
		EquivalenceFunction->EqualSimplify
	];
	
	Test[
		Axiloop`Private`ExpandIntegral[output],
		input,
		TestID->testid,
		EquivalenceFunction->EqualSimplify
	];
];


TestCase[
	l.x l.p / ((l-y).(l-y) (l-z).n),
	Axiloop`Private`KK[l, {x,p},{-y},{-z}],
	"Test 01"
];

TestCase[
	(l.p)^3 l.x / ((l-y).(l-y) (l-z).n),
	Axiloop`Private`KK[l, {p,p,p,x},{-y},{-z}],
	"Test 02"
];

TestCase[
	l.x / (l.l (l-y).(l-y) (l-z).n),
	Axiloop`Private`KK[l, {x},{-y,0},{-z}],
	"Test 03"
];

TestCase[
	l.x / ((l-p).(l-p) (l-y).(l-y) (l-z).n),
	Axiloop`Private`KK[l, {x},{-y,-p},{-z}],
	"Test 04"
];

TestCase[
	l.x / ((l+p).(l+p) (l-y).(l-y) (l-z).n),
	Axiloop`Private`KK[l, {x},{p,-y},{-z}],
	"Test 05"
]

TestCase[
	l.x / ((l-y).(l-y) l.n (l-z).n),
	Axiloop`Private`KK[l, {x},{-y},{-z,0}],
	"Test 06"
];

TestCase[
	l.x / ((l-y).(l-y) (l-z).n (l-p).n),
	Axiloop`Private`KK[l, {x},{-y},{-z,-p}],
	"Test 07"
];

TestCase[
	l.x / ((l-y).(l-y) (l-z).n (-l+p).n),
	- Axiloop`Private`KK[l, {x},{-y},{-p,-z}],
	"Test 08"
];

TestCase[
	l.x / ((l-y).(l-y) (l-z).n (l+p).n),
	Axiloop`Private`KK[l, {x},{-y},{-z,p}],
	"Test 09"
];
*)