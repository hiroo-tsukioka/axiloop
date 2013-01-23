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

Test[
	CollectLoopIntegrals[l.a / ((l-b).(l-b) l.n), l]
	,
	$$[{a},{-b},{0}]
	,
	TestID->"CollectLoopIntegrals-20130118-K7W6N0"
];

Test[
	CollectLoopIntegrals[l.a / ((l+b).n l.n), l]
	,
	$$[{a},{},{0,b}]
	,
	TestID->"CollectLoopIntegrals-20130118-M5Z0W1"
];


Test[
	CollectLoopIntegrals[l.x l.p / ((l-y).(l-y) (l-z).n), l]
	,
	$$[{p,x},{-y},{-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-W6T6U8"
];

Test[
	CollectLoopIntegrals[(l.p)^3 l.x / ((l-y).(l-y) (l-z).n), l]
	,
	$$[{p,p,p,x},{-y},{-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-E7R5K1"
];

Test[
	CollectLoopIntegrals[l.x / (l.l (l-y).(l-y) (l-z).n), l]
	,
	$$[{x},{0,-y},{-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-X6L5V9"
];

Test[
	CollectLoopIntegrals[l.x / ((l-p).(l-p) (l-y).(l-y) (l-z).n), l]
	,
	$$[{x},{-p,-y},{-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-D7Z3W6"
];

Test[
	CollectLoopIntegrals[l.x / ((l+p).(l+p) (l-y).(l-y) (l-z).n), l]
	,
	$$[{x},{p,-y},{-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-O0Z4U6"
]

Test[
	CollectLoopIntegrals[l.x / ((l-y).(l-y) l.n (l-z).n), l]
	,
	$$[{x},{-y},{0,-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-X5H3A1"
];

Test[
	CollectLoopIntegrals[l.x / ((l-y).(l-y) (l-z).n (l-p).n), l]
	,
	$$[{x},{-y},{-p,-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-W7H9E3"
];

Test[
	CollectLoopIntegrals[l.x / ((l-y).(l-y) (l-z).n (-l+p).n), l]
	,
	- $$[{x},{-y},{-p,-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-R1L8L5"
];

Test[
	CollectLoopIntegrals[l.x / ((l-y).(l-y) (l-z).n (l+p).n), l]
	,
	$$[{x},{-y},{p,-z}]
	,
	TestID->"CollectLoopIntegrals-20130123-T8U4X7"
];
