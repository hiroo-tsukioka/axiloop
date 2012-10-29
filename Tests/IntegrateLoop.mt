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

(*  Unit tests for Axiloop package.                                           *)

Get["Tests/main.mt"]


Test[
	Axiloop`Private`KK[l, {x1,l,x2},{y1,0,y2},{z}]
		//. Axiloop`Private`ReduceIntegralRules[l],
    Axiloop`Private`KK[l, {x1,x2},{y1,y2},{z}],
	TestID->"ReduceIntegralRules Test 01",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {x1,p,x2},{y1,p,y2},{z}]
		//. Axiloop`Private`ReduceIntegralRules[l],
    ( p.p Axiloop`Private`KK[l, {x1,x2},{y1,p,y2},{z}]
        + Axiloop`Private`KK[l, {x1,l,x2},{y1,p,y2},{z}]
        - Axiloop`Private`KK[l, {x1,x2},{y1,y2},{z}]
    ) / 2,
	TestID->"ReduceIntegralRules Test 02",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {x},{y},{p,k}]
		//. Axiloop`Private`ReduceIntegralRules[l][[1]],
    (Axiloop`Private`KK[l, {x},{y},{p}]
        - Axiloop`Private`KK[l, {x},{y},{k}]
    ) / (p.n - k.n),
	TestID->"ReduceIntegralRules Test 03",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {x1,n,x2},{y},{p}]
		//. Axiloop`Private`ReduceIntegralRules[l][[1]],
    Axiloop`Private`KK[l, {x1,x2},{y},{}]
        + p.n Axiloop`Private`KK[l, {x1,x2},{y},{p}]
    ,
	TestID->"ReduceIntegralRules Test 04",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {l},{p,y1,y2},{z}]
		//. Axiloop`Private`ReduceIntegralRules[l][[2]][[1]],
    Axiloop`Private`KK[l, {},{y1,y2},{z}]
        + 2 Axiloop`Private`KK[l, {p},{p,y1,y2},{z}]
        - p.p Axiloop`Private`KK[l, {},{p,y1,y2},{z}]
    ,
	TestID->"ReduceIntegralRules Test 05",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {},{y1,y2},{p}]
		//. Axiloop`Private`ReduceIntegralRules[l][[2]][[2]],
    Axiloop`Private`KK[l, {},{y1-p,y2-p},{0}],
	TestID->"ReduceIntegralRules Test 06",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {x},{y1,y2},{p}]
		//. Axiloop`Private`ReduceIntegralRules[l][[2]][[3]],
    Axiloop`Private`KK[l, {x},{y1-p,y2-p},{0}]
        + p.x Axiloop`Private`KK[l, {},{y1-p,y2-p},{0}],
	TestID->"ReduceIntegralRules Test 07",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`KK[l, {},{y},{0}]
		//. Axiloop`Private`ReduceIntegralRules[l][[3]],
    Axiloop`Private`KK[l, {},{0},{0}]
        + 2 Axiloop`Private`KK[l, {y},{y,0},{0}]
        - y.y Axiloop`Private`KK[l, {},{y,0},{0}],
	TestID->"ReduceIntegralRules Test 08",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[1/(l.l l.n), l]
	    + IntegrateLoop[1/(l-p).(l-p), l]
	    - IntegrateLoop[1/(l-k).(l-k), l],
	0,
	TestID->"K1(0;0)+I1(p)-I1(k)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k) l.n), l],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
	    (I0 + Log[x])/eta - I1 + Li2[1] + I0 Log[x] + (Log[x]^2)/2
	) / (x (k.k)^eta),
	TestID->"K2(k,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-p).(l-p) l.n), l],
    I (4*Pi)^(-2 + eta) Gamma[1 + eta] (I0/eta - I1 + Li2[1]) (p.p)^-eta,
	TestID->"K2(p,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/((l-k).(l-k) (l-p).(l-p) l.n), l],
	0,
	TestID->"K2(p,k;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k) l.n), l],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
	    2 Beta[1-eta, 1-eta] k.xx / k.n
	    + (2 - I0 - Log[x] + eta (4 + I1 - Li2[1] - I0 Log[x] - (Log[x]^2)/2))
	        * k.k n.xx / (k.n)^2
	) / (2 eta (k.k)^eta),
	TestID->"K2x(k,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-p).(l-p) l.n), l, Compact->True],
	I (4 Pi)^(-2 + eta) Gamma[1 + eta] (p.p)^-eta 1/p.n (
		Axiloop`Private`B1 p.xx + Axiloop`Private`B3 n.xx p.p/(2 p.n)
	),
	TestID->"K2x(p,0;0) compact",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-p).(l-p) l.n), l],
	I (4 Pi)^(-2 + eta) Gamma[1 + eta] (p.p)^-eta 1/p.n (
	    ((2 - I0)/eta + 4 + I1 - Li2[1]) n.xx p.p/(2 p.n)
	    + Beta[1-eta, 1-eta]/eta p.xx
	),
	TestID->"K2x(p,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k) (l-p).(l-p) l.n), l],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
	    1/eps^2 + (Log[x] - I0)/eps + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x]
	    - (Log[x]^2)/2
	) (k.k)^(-1-eta),
	TestID->"K3(p,k,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k) (l-p).(l-p) l.n), l, Compact->True],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
	    xx.p Axiloop`Private`S1
	    + xx.k Axiloop`Private`S2
	    + xx.n k.k/(2 k.n) Axiloop`Private`S3
	) (k.k)^(-1-eta),
	TestID->"K3x(p,k,0;0) compact",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k)), l],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Beta[1-eta, 1-eta] / (eta (k.k)^eta),
	TestID->"I2(y,0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/((l-p).(l-p) (l-k).(l-k)), l, Compact->True],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`T0 / (q.q)^eta,
	TestID->"I2(p,k) compact",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-p).(l-p) (l-k).(l-k)), l, Compact->True],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`R0 (k.k)^(-1-eta),
	TestID->"I3(p,k,0) compact",
	EquivalenceFunction->EqualSimplify
]
 
Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k)), l, Compact->True],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`T1 k.xx (k.k)^(-eta),
	TestID->"I2x(y,0) compact",
	EquivalenceFunction->EqualSimplify
]
 
Test[
	IntegrateLoop[l.xx/((l-p).(l-p) (l-k).(l-k)), l, Compact->True],
	I (4 Pi)^(-2 + eta) Gamma[1 + eta] (q.q)^-eta (
	    Axiloop`Private`T0 k.xx + Axiloop`Private`T1 (p.xx - k.xx)
	),
	TestID->"I2x(p,k) compact",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-p).(l-p) (l-k).(l-k)), l, Compact->True],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
	    xx.p Axiloop`Private`R1 + xx.k Axiloop`Private`R2
	) (k.k)^(-1-eta),
	TestID->"I3x(p,k,0) compact",
	EquivalenceFunction->EqualSimplify
]
 
Test[
	IntegrateLoop[l.xx l.yy / (l.l (l-p).(l-p) (l-k).(l-k)), l, Compact->True],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
				xx.p yy.p Axiloop`Private`R3 +
				xx.k yy.k Axiloop`Private`R4 +
				(xx.k yy.p + xx.p yy.k) Axiloop`Private`R5 +
				k.k xx.yy Axiloop`Private`R6
			) (k.k)^(-1-eta),
	TestID->"I3xy(p,k,0) compact",
	EquivalenceFunction->EqualSimplify
]