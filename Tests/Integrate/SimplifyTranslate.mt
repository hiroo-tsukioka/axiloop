(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2013 Oleksandr Gituliar.                                    *)
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

Needs["Axiloop`Integrate`"]

Get["Tests/core.mt"]


SimplifyTranslate = Axiloop`Integrate`Private`$$SimplifyTranslate;


Test[
	SimplifyTranslate[$$[{},{p},{k}]]
	,
	$$[{},{q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-O7H2C5"
];

Test[
	SimplifyTranslate[$$[{},{p},{p}]]
	,
	$$[{},{0},{0}]
	,
	TestID->"SimplifyTranslate-20130226-A0X1K2"
];

Test[
	SimplifyTranslate[$$[{},{0,k},{k}]]
	,
	- $$[{},{0,k},{0}]
	,
	TestID->"SimplifyTranslate-20130226-V6B9W9"
];

Test[
	SimplifyTranslate[$$[{},{0,k},{p}]]
	,
	- $$[{},{p,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-Q3L5K8"
];

Test[
	SimplifyTranslate[$$[{},{0,p},{k}]]
	,
	- $$[{},{k,-q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-J4S8P6"
];

Test[
	SimplifyTranslate[$$[{},{0,p},{p}]]
	,
	- $$[{},{0,p},{0}]
	,
	TestID->"SimplifyTranslate-20130226-Q8C7L5"
];

Test[
	SimplifyTranslate[$$[{}, {k, p}, {k}]]
	,
	$$[{},{0,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-X4T8C2"
];

Test[
	SimplifyTranslate[$$[{}, {k, p}, {p}]]
	,
	- $$[{},{0,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-K4P7Y0"
];

Test[
	SimplifyTranslate[$$[{}, {0, k, p}, {k}]]
	,
	- $$[{},{0,k,-q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-L1L5W3"
];

Test[
	SimplifyTranslate[$$[{}, {0, k, p}, {p}]]
	,
	- $$[{},{0,p,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-G7J6L7"
];

Test[
	SimplifyTranslate[$$[{k}, {0, k}, {k}]]
	,
	$$[{k},{0,k},{0}] + k.k $$[{},{0,k},{0}]
	,
	TestID->"SimplifyTranslate-20130226-S9E3H9"
];

Test[
	SimplifyTranslate[$$[{k}, {0, k}, {p}]]
	,
	$$[{k},{p,q},{0}] + k.p $$[{},{p,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-U9E5D3"
];

Test[
	SimplifyTranslate[$$[{k}, {0, p}, {p}]]
	,
	$$[{k},{0,p},{0}] + k.p $$[{},{0,p},{0}]
	,
	TestID->"SimplifyTranslate-20130226-J7Z5U4"
];

Test[
	SimplifyTranslate[$$[{k}, {k, p}, {k}]]
	,
	$$[{k},{0,q},{0}] - k.k $$[{},{0,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-A4R2M6"
];

Test[
	SimplifyTranslate[$$[{k}, {k, p}, {p}]]
	,
	$$[{k},{0,q},{0}] + k.p $$[{},{0,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-O7Q9M9"
];

Test[
	SimplifyTranslate[$$[{p}, {0, k}, {k}]]
	,
	$$[{p},{0,k},{0}] + k.p $$[{},{0,k},{0}]
	,
	TestID->"SimplifyTranslate-20130226-D1O2B5"
];

Test[
	SimplifyTranslate[$$[{p}, {k, p}, {k}]]
	,
	$$[{p},{0,q},{0}] - k.p $$[{},{0,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-Y9A1D3"
];

Test[
	SimplifyTranslate[$$[{p}, {k, p}, {p}]]
	,
	$$[{p},{0,q},{0}] + p.p $$[{},{0,q},{0}]
	,
	TestID->"SimplifyTranslate-20130226-G8R5Q8"
];
