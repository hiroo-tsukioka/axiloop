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


SimplifyLoopIntegrals = Axiloop`Private`SimplifyLoopIntegrals;
$$ = Axiloop`Private`$$;


Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{}]]
	,
	$$[{},{k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-Q9S3X3"
];

Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{0}]]
	,
	- $$[{},{k,p},{0}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-L7J2Y1"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{-k,-p},{}]]
	,
	- $$[{k},{k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-K2B6L1"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{-k,-p},{0}]]
	,
	$$[{k},{k,p},{0}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-U9S0E8"
];


Test[
	SimplifyLoopIntegrals[$$[{},{0,-k,-p},{}]]
	,
	$$[{},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-F5T7E6"
];


Test[
	SimplifyLoopIntegrals[$$[{},{0,-k,-p},{0}]]
	,
	- $$[{},{0,k,p},{0}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-O9T5L8"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{0,-k,-p},{}]]
	,
	- $$[{k},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-B4D4M8"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{0,-k,-p},{0}]]
	,
	1/2 ($$[{}, {0,p}, {0}] - $$[{}, {k,p}, {0}] - k.k $$[{}, {0,k,p}, {0}])
	,
	TestID->"SimplifyLoopIntegrals-20130219-H2C8B2"
];

Test[
	SimplifyLoopIntegrals[$$[{p},{0,-k,-p},{}]]
	,
	- $$[{p},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-O6H1K0"
];

Test[
	SimplifyLoopIntegrals[$$[{p},{0,-k,-p},{0}]]
	,
	1/2 ($$[{}, {0,k}, {0}] - $$[{}, {k,p}, {0}] - p.p $$[{}, {0,k,p}, {0}])
	,
	TestID->"SimplifyLoopIntegrals-20130219-N1S5N0"
];

Test[
	SimplifyLoopIntegrals[$$[{k,k},{0,-k,-p},{}]]
	,
	$$[{k,k},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-V4O2D6"
];

Test[
	SimplifyLoopIntegrals[$$[{k,k},{0,-k,-p},{0}]]
	,
	- 1/2 (
		$$[{k},{0,p},{0}]
		- k.k/2 (
			$$[{}, {0,p}, {0}]
			- $$[{}, {k,p}, {0}]
			- k.k $$[{}, {0,k,p}, {0}])
		- $$[{k},{k,p},{0}]
	)
	,
	TestID->"SimplifyLoopIntegrals-20130219-J1A9X5"
];


Test[
	SimplifyLoopIntegrals[$$[{k,p},{0,-k,-p},{}]]
	,
	$$[{k,p},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130219-U4I6W6"
];

Test[
	SimplifyLoopIntegrals[$$[{k,p},{0,-k,-p},{0}]]
	,
	- 1/2 (
		$$[{p},{0,p},{0}]
		- k.k/2 (
			$$[{}, {0,k}, {0}]
			- $$[{}, {k,p}, {0}]
			- p.p $$[{}, {0,k,p}, {0}]
		)
		- $$[{p},{k,p},{0}]
	)
	,
	TestID->"SimplifyLoopIntegrals-20130219-I1D8P3"
];


Test[
	SimplifyLoopIntegrals[$$[{k,k},{0,-k,-p},{0}]]
	,
	- 1/2 (
		$$[{k},{0,p},{0}]
		- k.k/2 (
			$$[{}, {0,p}, {0}]
			- $$[{}, {k,p}, {0}]
			- k.k $$[{}, {0,k,p}, {0}]
		)
		- $$[{k},{k,p},{0}]
	)
	,
	TestID->"SimplifyLoopIntegrals-20130219-Y2B2S6"
];


Test[
	SimplifyLoopIntegrals[$$[{},{k,p},{0,k}]]
	,
	($$[{}, {k,p}, {0}] - $$[{}, {k,p}, {k}]) / k.n
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-U2S3P5"
];

Test[
	SimplifyLoopIntegrals[$$[{},{k,p},{k,p}]]
	,
	($$[{}, {k,p}, {p}] - $$[{}, {k,p}, {k}]) / (k.n-n.p)
	,
	TestID->"SimplifyLoopIntegrals-20130219-X1Q4T2"
];

Test[
	SimplifyLoopIntegrals[$$[{},{k,p},{0,k,p}]]
	,
	(
		  ($$[{},{k,p},{0}] - $$[{},{k,p},{p}]) / n.p
		- ($$[{},{k,p},{p}] - $$[{},{k,p},{k}]) / (k.n - n.p)
	) / k.n
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-V1Z3G1"
];


Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{}]]
	,
	$$[{},{k,p},{}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130206-M2T2O2"
];

Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{-k}]]
	,
	- $$[{},{k,p},{k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-B3C7L6"
];

Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{-p}]]
	,
	- $$[{},{k,p},{p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-A5Z4U9"
];

Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{-k,-p}]]
	,
	($$[{},{k,p},{k}] - $$[{},{k,p},{p}]) / (p.n - k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-I5J7V9"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{-k,-p},{-k}]]
	,
	$$[{k},{k,p},{k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-F8L1R6"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{-k,-p},{-p}]]
	,
	$$[{k},{k,p},{p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-H8Q1Q2"
];

Test[
	SimplifyLoopIntegrals[$$[{k}, {-k,-p}, {-k,-p}]]
	,
	- ($$[{k},{k,p},{k}] - $$[{k},{k,p},{p}]) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-O1K2B0"
];

Test[
	SimplifyLoopIntegrals[$$[{k}, {0,-k,-p}, {}]]
	,
	- $$[{k}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-X8E0W1"
];

Test[
	SimplifyLoopIntegrals[$$[{k}, {0,-k,-p}, {-k}]]
	,
	1/2 (
		$$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}]
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-Q6L8X5"
];

Test[
	SimplifyLoopIntegrals[$$[{k}, {0,-k,-p}, {-p}]]
	,
	1/2 (
		$$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}]
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-T0J5R4"
];

Test[
	SimplifyLoopIntegrals[$$[{k}, {0,-k,-p}, {-k,-p}]]
	,
	- 1/2 (
		 $$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}]
		- 
		($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-X1K0G7"
];

Test[
	SimplifyLoopIntegrals[$$[{l}, {-k, -p}, {-k, -p}]]
	,
	(
		$$[{}, {p}, {k}] - 2 $$[{k}, {k,p}, {k}] - k.k $$[{}, {k,p}, {k}]
		- ($$[{}, {p}, {p}] - 2 $$[{k}, {k,p}, {p}] - k.k $$[{}, {k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130219-G3S5P4"
];

(*

$$[{n}, {-k, -p}, {-k}],$$[{n}, {-k, -p}, {-p}],$$[{n}, {-k, -p}, {-k, -p}],$$[{n}, {0, -k, -p}, {}],$$[{n}, {0, -k, -p}, {-k}],$$[{n}, {0, -k, -p}, {-p}],$$[{n}, {0, -k, -p}, {-k, -p}],$$[{p}, {-k, -p}, {-k}],$$[{p}, {-k, -p}, {-p}],$$[{p}, {-k, -p}, {-k, -p}],$$[{p}, {0, -k, -p}, {}],$$[{p}, {0, -k, -p}, {-k}],$$[{p}, {0, -k, -p}, {-p}],$$[{p}, {0, -k, -p}, {-k, -p}],$$[{k, k}, {0, -k, -p}, {}],$$[{k, k}, {0, -k, -p}, {-p}],$$[{k, n}, {-k, -p}, {-k, -p}],$$[{k, n}, {0, -k, -p}, {}],$$[{k, n}, {0, -k, -p}, {-k}],$$[{k, n}, {0, -k, -p}, {-p}],$$[{k, n}, {0, -k, -p}, {-k, -p}],$$[{k, p}, {0, -k, -p}, {}],$$[{k, p}, {0, -k, -p}, {-k}],$$[{k, p}, {0, -k, -p}, {-p}],$$[{n, n}, {-k, -p}, {-k, -p}],$$[{n, n}, {0, -k, -p}, {}],$$[{n, n}, {0, -k, -p}, {-k}],$$[{n, n}, {0, -k, -p}, {-p}],$$[{n, n}, {0, -k, -p}, {-k, -p}],$$[{n, p}, {-k, -p}, {-k, -p}],$$[{n, p}, {0, -k, -p}, {}],$$[{n, p}, {0, -k, -p}, {-k}],$$[{n, p}, {0, -k, -p}, {-p}],$$[{n, p}, {0, -k, -p}, {-k, -p}],$$[{p, p}, {0, -k, -p}, {}],$$[{p, p}, {0, -k, -p}, {-k}],$$[{k, n, n}, {0, -k, -p}, {-k, -p}],$$[{n, n, n}, {0, -k, -p}, {-k, -p}],$$[{n, n, p}, {0, -k, -p}, {-k, -p}]

*)
