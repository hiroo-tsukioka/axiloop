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
	TestID->"SimplifyLoopIntegrals-20130117-B1H3G4"
];

Test[
	SimplifyLoopIntegrals[$$[{},{-k,-p},{0}]]
	,
	- $$[{},{k,p},{0}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-B2U1W3"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{-k,-p},{}]]
	,
	- $$[{k},{k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-U2P2R8"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{-k,-p},{0}]]
	,
	$$[{k},{k,p},{0}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-L1U7I2"
];


Test[
	SimplifyLoopIntegrals[$$[{},{0,-k,-p},{}]]
	,
	$$[{},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-B2U5Q8"
];


Test[
	SimplifyLoopIntegrals[$$[{},{0,-k,-p},{0}]]
	,
	- $$[{},{0,k,p},{0}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-Q2C4X0"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{0,-k,-p},{}]]
	,
	- $$[{k},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-W0B9Z1"
];

Test[
	SimplifyLoopIntegrals[$$[{k},{0,-k,-p},{0}]]
	,
	1/2 ($$[{}, {0,p}, {0}] - $$[{}, {k,p}, {0}] - k.k $$[{}, {0,k,p}, {0}])
	,
	TestID->"SimplifyLoopIntegrals-20130123-V6B3X2"
];

Test[
	SimplifyLoopIntegrals[$$[{p},{0,-k,-p},{}]]
	,
	- $$[{p},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-V5N6K9"
];

Test[
	SimplifyLoopIntegrals[$$[{p},{0,-k,-p},{0}]]
	,
	1/2 ($$[{}, {0,k}, {0}] - $$[{}, {k,p}, {0}] - p.p $$[{}, {0,k,p}, {0}])
	,
	TestID->"SimplifyLoopIntegrals-20130123-A0S1X2"
];

Test[
	SimplifyLoopIntegrals[$$[{k,k},{0,-k,-p},{}]]
	,
	$$[{k,k},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-W6B3O5"
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
	TestID->"SimplifyLoopIntegrals-20130123-P1L9R7"
];


Test[
	SimplifyLoopIntegrals[$$[{k,p},{0,-k,-p},{}]]
	,
	$$[{k,p},{0,k,p},{}]
	,
	TestID->"SimplifyLoopIntegrals-20130123-C5P5S4"
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
	TestID->"SimplifyLoopIntegrals-20130123-K3D1W7"
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
	TestID->"SimplifyLoopIntegrals-20130123-G6C2C2"
];


Test[
	SimplifyLoopIntegrals[$$[{},{k,p},{0,k}]]
	,
	($$[{}, {k,p}, {0}] - $$[{}, {k,p}, {k}]) / k.n
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyLoopIntegrals-20130206-K0Y4F2"
];

Test[
	SimplifyLoopIntegrals[$$[{},{k,p},{k,p}]]
	,
	($$[{}, {k,p}, {p}] - $$[{}, {k,p}, {k}]) / (k.n-n.p)
	,
	TestID->"SimplifyLoopIntegrals-20130206-R4C0N5"
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
	TestID->"SimplifyLoopIntegrals-20130206-M2T2O2"
];