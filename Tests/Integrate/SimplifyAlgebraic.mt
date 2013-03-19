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

Needs["Axiloop`Integrtate`"]

Get["Tests/core.mt"]


SimplifyAlgebraic = Axiloop`Integrate`Private`$$SimplifyAlgebraic;


Test[
	SimplifyAlgebraic[$$[{},{-k,-p},{}]]
	,
	$$[{},{k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-Q9S3X3"
];

Test[
	SimplifyAlgebraic[$$[{},{-k,-p},{0}]]
	,
	- $$[{},{k,p},{0}]
	,
	TestID->"SimplifyAlgebraic-20130219-L7J2Y1"
];

Test[
	SimplifyAlgebraic[$$[{k},{-k,-p},{}]]
	,
	- $$[{k},{k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-K2B6L1"
];

Test[
	SimplifyAlgebraic[$$[{k},{-k,-p},{0}]]
	,
	$$[{k},{k,p},{0}]
	,
	TestID->"SimplifyAlgebraic-20130219-U9S0E8"
];


Test[
	SimplifyAlgebraic[$$[{},{0,-k,-p},{}]]
	,
	$$[{},{0,k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-F5T7E6"
];


Test[
	SimplifyAlgebraic[$$[{},{0,-k,-p},{0}]]
	,
	- $$[{},{0,k,p},{0}]
	,
	TestID->"SimplifyAlgebraic-20130219-O9T5L8"
];

Test[
	SimplifyAlgebraic[$$[{k},{0,-k,-p},{}]]
	,
	- $$[{k},{0,k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-B4D4M8"
];

Test[
	SimplifyAlgebraic[$$[{k},{0,-k,-p},{0}]]
	,
	1/2 ($$[{}, {0,p}, {0}] - $$[{}, {k,p}, {0}] - k.k $$[{}, {0,k,p}, {0}])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-H2C8B2"
];

Test[
	SimplifyAlgebraic[$$[{p},{0,-k,-p},{}]]
	,
	- $$[{p},{0,k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-O6H1K0"
];

Test[
	SimplifyAlgebraic[$$[{p},{0,-k,-p},{0}]]
	,
	1/2 ($$[{}, {0,k}, {0}] - $$[{}, {k,p}, {0}] - p.p $$[{}, {0,k,p}, {0}])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-N1S5N0"
];

Test[
	SimplifyAlgebraic[$$[{k,k},{0,-k,-p},{}]]
	,
	$$[{k,k},{0,k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-V4O2D6"
];

Test[
	SimplifyAlgebraic[$$[{k,k},{0,-k,-p},{0}]]
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
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-J1A9X5"
];


Test[
	SimplifyAlgebraic[$$[{k,p},{0,-k,-p},{}]]
	,
	$$[{k,p},{0,k,p},{}]
	,
	TestID->"SimplifyAlgebraic-20130219-U4I6W6"
];

Test[
	SimplifyAlgebraic[$$[{k,p},{0,-k,-p},{0}]]
	,
	- 1/2 (
		$$[{k},{0,k},{0}]
		- $$[{k},{k,p},{0}]
		- p.p 1/2 (
			$$[{}, {0,p}, {0}]
			- $$[{}, {k,p}, {0}]
			- k.k $$[{}, {0,k,p}, {0}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-I1D8P3"
];


Test[
	SimplifyAlgebraic[$$[{k,k},{0,-k,-p},{0}]]
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
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-Y2B2S6"
];


Test[
	SimplifyAlgebraic[$$[{},{k,p},{0,k}]]
	,
	($$[{}, {k,p}, {0}] - $$[{}, {k,p}, {k}]) / k.n
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-U2S3P5"
];

Test[
	SimplifyAlgebraic[$$[{},{k,p},{k,p}]]
	,
	($$[{}, {k,p}, {p}] - $$[{}, {k,p}, {k}]) / (k.n-n.p)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-X1Q4T2"
];

Test[
	SimplifyAlgebraic[$$[{},{k,p},{0,k,p}]]
	,
	(
		  ($$[{},{k,p},{0}] - $$[{},{k,p},{p}]) / n.p
		- ($$[{},{k,p},{p}] - $$[{},{k,p},{k}]) / (k.n - n.p)
	) / k.n
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-V1Z3G1"
];


Test[
	SimplifyAlgebraic[$$[{},{-k,-p},{}]]
	,
	$$[{},{k,p},{}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130206-M2T2O2"
];

Test[
	SimplifyAlgebraic[$$[{},{-k,-p},{-k}]]
	,
	- $$[{},{k,p},{k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-B3C7L6"
];

Test[
	SimplifyAlgebraic[$$[{},{-k,-p},{-p}]]
	,
	- $$[{},{k,p},{p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-A5Z4U9"
];

Test[
	SimplifyAlgebraic[$$[{},{-k,-p},{-k,-p}]]
	,
	($$[{},{k,p},{k}] - $$[{},{k,p},{p}]) / (p.n - k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-I5J7V9"
];

Test[
	SimplifyAlgebraic[$$[{k},{-k,-p},{-k}]]
	,
	$$[{k},{k,p},{k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-F8L1R6"
];

Test[
	SimplifyAlgebraic[$$[{k},{-k,-p},{-p}]]
	,
	$$[{k},{k,p},{p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-H8Q1Q2"
];

Test[
	SimplifyAlgebraic[$$[{k}, {-k,-p}, {-k,-p}]]
	,
	- ($$[{k},{k,p},{k}] - $$[{k},{k,p},{p}]) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-O1K2B0"
];

Test[
	SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {}]]
	,
	- $$[{k}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-X8E0W1"
];

Test[
	SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {-k}]]
	,
	1/2 (
		$$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}]
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-Q6L8X5"
];

Test[
	SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {-p}]]
	,
	1/2 (
		$$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}]
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-T0J5R4"
];

Test[
	SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {-k,-p}]]
	,
	- 1/2 (
		 $$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}]
		- 
		($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-X1K0G7"
];

Test[
	SimplifyAlgebraic[$$[{l}, {-k, -p}, {-k, -p}]]
	,
	(
		$$[{}, {p}, {k}] - 2 $$[{k}, {k,p}, {k}] - k.k $$[{}, {k,p}, {k}]
		- ($$[{}, {p}, {p}] - 2 $$[{k}, {k,p}, {p}] - k.k $$[{}, {k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-G3S5P4"
];

Test[
	SimplifyAlgebraic[$$[{n}, {-k, -p}, {-k}]]
	,
	$$[{}, {k,p}, {}] - k.n $$[{}, {k,p}, {k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-Y4X0M1"
];

Test[
	SimplifyAlgebraic[$$[{n}, {-k, -p}, {-p}]]
	,
	$$[{}, {k,p}, {}] - p.n $$[{}, {k,p}, {p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-B3K5C3"
];

Test[
	SimplifyAlgebraic[$$[{n}, {-k, -p}, {-k, -p}]]
	,
	- ($$[{}, {k,p}, {}] - k.n $$[{}, {k,p}, {k}]
	    - ($$[{}, {k,p}, {}] - p.n $$[{}, {k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-R7J6U7"
];

Test[
	SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {}]]
	,
	- $$[{n}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-G7J4C7"
];

Test[
	SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {-k}]]
	,
	$$[{}, {0,k,p}, {}] - k.n $$[{}, {0,k,p}, {k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-R2G3Z4"
];

Test[
	SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {-p}]]
	,
	$$[{}, {0,k,p}, {}] - p.n $$[{}, {0,k,p}, {p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-R1C2H1"
];

Test[
	SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {-k, -p}]]
	,
	- ($$[{}, {0,k,p}, {}] - k.n $$[{}, {0,k,p}, {k}]
	    - ($$[{}, {0,k,p}, {}] - p.n $$[{}, {0,k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-B7K9H6"
];

Test[
	SimplifyAlgebraic[$$[{p}, {-k, -p}, {-k}]]
	,
	$$[{p}, {k,p}, {k}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-X5N2P6"
];

Test[
	SimplifyAlgebraic[$$[{p}, {-k, -p}, {-p}]]
	,
	$$[{p}, {k,p}, {p}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-D5R6W2"
];

Test[
	SimplifyAlgebraic[$$[{p}, {-k, -p}, {-k, -p}]]
	,
	- ($$[{p}, {k,p}, {k}] - $$[{p}, {k,p}, {p}]) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-Z0C7I5"
];

Test[
	SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {}]]
	,
	- $$[{p}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-E0X0F7"
];

Test[
	SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {-k}]]
	,
	1/2 (
		$$[{}, {0,k}, {k}] - $$[{}, {k,p}, {k}] - p.p $$[{}, {0,k,p}, {k}]
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-E7L0E3"
];

Test[
	SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {-p}]]
	,
	1/2 (
		$$[{}, {0,k}, {p}] - $$[{}, {k,p}, {p}] - p.p $$[{}, {0,k,p}, {p}]
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-B0R1V9"
];

Test[
	SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {-k, -p}]]
	,
	- 1/2 (
		$$[{}, {0,k}, {k}] - $$[{}, {k,p}, {k}] - p.p $$[{}, {0,k,p}, {k}]
			- ($$[{}, {0,k}, {p}] - $$[{}, {k,p}, {p}] - p.p $$[{}, {0,k,p}, {p}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-O0Q0R9"
];

Test[
	SimplifyAlgebraic[$$[{k, k}, {0, -k, -p}, {}]]
	,
	$$[{k,k}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-C8Y5P9"
];

Test[
	SimplifyAlgebraic[$$[{k, k}, {0, -k, -p}, {-p}]]
	,
	- 1/2 (
		$$[{k}, {0,p}, {p}] - $$[{k}, {k,p}, {p}]
			- k.k 1/2 ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}] )
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-S4T0X4"
];

Test[
	SimplifyAlgebraic[$$[{k, n}, {-k, -p}, {-k, -p}]]
	,
	(- k.n $$[{k}, {k,p}, {k}] + p.n $$[{k}, {k,p}, {p}]) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-O3S1G5"
];

Test[
	SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {}]]
	,
	$$[{k,n}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-F9E6K3"
];

Test[
	SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {-k}]]
	,
	- ($$[{k}, {0,k,p}, {}]
		- k.n 1/2 ($$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}])
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-K0G7V6"
];

Test[
	SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {-p}]]
	,
	- ($$[{k}, {0,k,p}, {}]
		- p.n 1/2 ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-D9J9V9"
];

Test[
	SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {-k, -p}]]
	,
	(
		($$[{k}, {0,k,p}, {}]
			- k.n 1/2 ($$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}])
		)
		-
		($$[{k}, {0,k,p}, {}]
			- p.n 1/2 ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-N6D1S3"
];

Test[
	SimplifyAlgebraic[$$[{k, p}, {0, -k, -p}, {}]]
	,
	$$[{k,p}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130219-C2C2G7"
];

Test[
	SimplifyAlgebraic[$$[{k, p}, {0, -k, -p}, {-k}]]
	,
	- 1/2 (
		$$[{k}, {0,k}, {k}]
		- $$[{k}, {k,p}, {k}]
		- p.p 1/2 (
			 $$[{}, {0,p}, {k}]
			 - $$[{}, {k,p}, {k}]
			 - k.k $$[{}, {0,k,p}, {k}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-M4I2I5"
];

Test[
	SimplifyAlgebraic[$$[{k, p}, {0, -k, -p}, {-p}]]
	,
	- 1/2 (
		$$[{k}, {0,k}, {p}]
		- $$[{k}, {k,p}, {p}]
		- p.p 1/2 (
			 $$[{}, {0,p}, {p}]
			 - $$[{}, {k,p}, {p}]
			 - k.k $$[{}, {0,k,p}, {p}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-V0V9I9"
];

Test[
	SimplifyAlgebraic[$$[{n, n}, {-k, -p}, {-k, -p}]]
	,
	(
		p.n ($$[{}, {k,p}, {}] - p.n $$[{}, {k,p}, {p}])
		-
		k.n ($$[{}, {k,p}, {}] - k.n $$[{}, {k,p}, {k}])
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-L6X1B2"
];

Test[
	SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {}]]
	,
	$$[{n,n}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-E7X1R2"
];

Test[
	SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {-k}]]
	,
	- (
		$$[{n}, {0,k,p}, {}]
		-
		k.n (
			$$[{}, {0,k,p}, {}]
			-
			k.n $$[{}, {0,k,p}, {k}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-R0X8Z7"
];

Test[
	SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {-p}]]
	,
	- (
		$$[{n}, {0,k,p}, {}]
		-
		p.n (
			$$[{}, {0,k,p}, {}]
			-
			p.n $$[{}, {0,k,p}, {p}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-E6K7T9"
];

Test[
	SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {-k, -p}]]
	,
	(
		(
			$$[{n}, {0,k,p}, {}]
			-
			k.n (
				$$[{}, {0,k,p}, {}]
				-
				k.n $$[{}, {0,k,p}, {k}]
			)
		)
		-
		(
			$$[{n}, {0,k,p}, {}]
			-
			p.n (
				$$[{}, {0,k,p}, {}]
				-
				p.n $$[{}, {0,k,p}, {p}]
			)
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-X2P5T1"
];

Test[
	SimplifyAlgebraic[$$[{n, p}, {-k, -p}, {-k, -p}]]
	,
	(
		(
			$$[{p}, {k,p}, {}]
			-
			k.n $$[{p}, {k,p}, {k}]
		)
		-
		(
			$$[{p}, {k,p}, {}]
			-
			p.n $$[{p}, {k,p}, {p}]
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-X3Y7P4"
];

Test[
	SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {}]]
	,
	$$[{n,p}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-O3F6W3"
];

Test[
	SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {-k}]]
	,
	- (
		$$[{p}, {0,k,p}, {}]
		-
		k.n 1/2 (
			$$[{}, {0,k}, {k}]
			-
			$$[{}, {k,p}, {k}]
			-
			p.p $$[{}, {0,k,p}, {k}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-Q7E2S6"
];

Test[
	SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {-p}]]
	,
	- (
		$$[{p}, {0,k,p}, {}]
		-
		p.n 1/2 (
			$$[{}, {0,k}, {p}]
			-
			$$[{}, {k,p}, {p}]
			-
			p.p $$[{}, {0,k,p}, {p}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-X3J6D4"
];

Test[
	SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {-k, -p}]]
	,
	(
		(
			$$[{p}, {0,k,p}, {}]
			-
			k.n 1/2 (
				$$[{}, {0,k}, {k}]
				-
				$$[{}, {k,p}, {k}]
				-
				p.p $$[{}, {0,k,p}, {k}]
			)
		)
		-
		(
			$$[{p}, {0,k,p}, {}]
			-
			p.n 1/2 (
				$$[{}, {0,k}, {p}]
				-
				$$[{}, {k,p}, {p}]
				-
				p.p $$[{}, {0,k,p}, {p}]
			)
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-K5S8D8"
];

Test[
	SimplifyAlgebraic[$$[{p, p}, {0, -k, -p}, {}]]
	,
	$$[{p,p}, {0,k,p}, {}]
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-W7X6B1"
];

Test[
	SimplifyAlgebraic[$$[{p, p}, {0, -k, -p}, {-k}]]
	,
	- 1/2 (
		$$[{p}, {0,k}, {k}]
		-
		$$[{p}, {k,p}, {k}]
		-
		p.p 1/2 (
			$$[{}, {0,k}, {k}]
			-
			$$[{}, {k,p}, {k}]
			-
			p.p $$[{}, {0,k,p}, {k}]
		)
	)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-O9H3G8"
];

Test[
	SimplifyAlgebraic[$$[{k, n, n}, {0, -k, -p}, {-k, -p}]]
	,
	- (
		(
			$$[{k,n}, {0,k,p}, {}]
			-
			k.n (
				$$[{k}, {0,k,p}, {}]
				-
				k.n 1/2 (
					$$[{}, {0,p}, {k}]
					-
					$$[{}, {k,p}, {k}]
					-
					k.k $$[{}, {0,k,p}, {k}]
				)
			)
		)
		-
		(
			$$[{k,n}, {0,k,p}, {}]
			-
			p.n (
				$$[{k}, {0,k,p}, {}]
				-
				p.n 1/2 (
					$$[{}, {0,p}, {p}]
					-
					$$[{}, {k,p}, {p}]
					-
					k.k $$[{}, {0,k,p}, {p}]
				)
			)
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-B5Q0M5"
];

Test[
	SimplifyAlgebraic[$$[{n, n, n}, {0, -k, -p}, {-k, -p}]]
	,
	- (
		(
			$$[{n,n}, {0,k,p}, {}]
			-
			k.n (
				$$[{n}, {0,k,p}, {}]
				-
				k.n (
					$$[{}, {0,k,p}, {}]
					-
					k.n $$[{}, {0,k,p}, {k}]
				)
			)
		)
		-
		(
			$$[{n,n}, {0,k,p}, {}]
			-
			p.n (
				$$[{n}, {0,k,p}, {}]
				-
				p.n (
					$$[{}, {0,k,p}, {}]
					-
					p.n $$[{}, {0,k,p}, {p}]
				)
			)
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-X6J5S7"
];

Test[
	SimplifyAlgebraic[$$[{n, n, p}, {0, -k, -p}, {-k, -p}]]
	,
	- (
		(
			$$[{n,p}, {0,k,p}, {}]
			-
			k.n (
				$$[{p}, {0,k,p}, {}]
				-
				k.n 1/2 (
					$$[{}, {0,k}, {k}]
					-
					$$[{}, {k,p}, {k}]
					-
					p.p $$[{}, {0,k,p}, {k}]
				)
			)
		)
		-
		(
			$$[{n,p}, {0,k,p}, {}]
			-
			p.n (
				$$[{p}, {0,k,p}, {}]
				-
				p.n 1/2 (
					$$[{}, {0,k}, {p}]
					-
					$$[{}, {k,p}, {p}]
					-
					p.p $$[{}, {0,k,p}, {p}]
				)
			)
		)
	) / (p.n-k.n)
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID->"SimplifyAlgebraic-20130220-J5P8O7"
];
