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
	IntegrateLoop[1/(l.l l.n), l] + IntegrateLoop[1/(l-p).(l-p), l] - IntegrateLoop[1/(l-k).(l-k), l],
	{
		{"compact", 0},
		{"expanded", 0}
	},
	TestID->"K1(0;0)+I1(p)-I1(k)",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[1/(l.l (l-k).(l-k) l.n), l],
	{
		{"compact", I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`P0 / (x (k.k)^eta)},
		{"expanded", I (4 Pi)^(-2+eta) Gamma[1+eta] ((I0 + Log[x]) / eta - I1 + Li2[1] + I0 Log[x] + (Log[x]^2)/2) / (x (k.k)^eta)}
	},
	TestID->"K2(k,0;0)",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[1/(l.l (l-p).(l-p) l.n), l],
	{
		{"compact", I Axiloop`Private`B0 (4*Pi)^(-2 + eta) Gamma[1 + eta] (p.p)^-eta},
		{"expanded", I (4*Pi)^(-2 + eta) Gamma[1 + eta] (I0/eta - I1 + Li2[1]) (p.p)^-eta}
	},
	TestID->"K2(p,0;0)",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[1/((l-k).(l-k) (l-p).(l-p) l.n), l],
	{
		{"compact", 0},
		{"expanded", 0}
	},
	TestID->"K2(p,k;0)",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[1/(l.l (l-k).(l-k) (l-p).(l-p) l.n), l],
	{
		{"compact", I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`S0 (k.k)^(-1-eta)},
		{"expanded", I (4 Pi)^(-2+eta) Gamma[1+eta] ( 1/eta^2 + (Log[x] - I0)/eta + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x] - (Log[x]^2)/2)  (k.k)^(-1-eta)}
	},
	TestID->"K3(p,k,0;0)",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k) l.n), l],
	{
		{"compact", I (4 Pi)^(-2+eta) Gamma[1+eta] (2 Axiloop`Private`P1 k.xx / k.n + Axiloop`Private`P3 k.k n.xx / (k.n)^2) / (2 (k.k)^eta)},
		{"expanded", I (4 Pi)^(-2+eta) Gamma[1+eta] (2 Beta[1-eta, 1-eta] k.xx / k.n + (2 - I0 - Log[x] + eta (4 + I1 - Li2[1] - I0 Log[x] - (Log[x]^2)/2)) k.k n.xx / (k.n)^2) / (2 eta (k.k)^eta)}
	},
	TestID->"K2x(k,0;0)",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[l.xx/(l.l (l-p).(l-p) l.n), l];
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2 + eta) Gamma[1 + eta] (p.p)^-eta ( 1/2 Axiloop`Private`B3 n.xx p.p / (p.n)^2 + Axiloop`Private`B1 p.xx / p.n),
	TestID->"K2x(p,0;0) compact",
	EquivalenceFunction->EqualSimplify
]
Test[
	GetValue[expr, "expanded"],
	I (4 Pi)^(-2 + eta) Gamma[1 + eta] (p.p)^-eta (1/2 ((2 - I0)/eta + 4 + I1 - Li2[1]) n.xx p.p / (p.n)^2 + Beta[1-eta, 1-eta]/eta p.xx / p.n),
	TestID->"K2x(p,0;0) expanded",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[l.xx/(l.l (l-k).(l-k) (l-p).(l-p) l.n), l];
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (xx.p Axiloop`Private`S1 + xx.k Axiloop`Private`S2 + xx.n k.k/(2 k.n) Axiloop`Private`S3 )  (k.k)^(-1-eta),
	TestID->"K3x(p,k,0;0) compact",
	EquivalenceFunction->EqualSimplify
]


Test[
	IntegrateLoop[1/(l.l (l-k).(l-k)), l],
	{
		{"compact", I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`T0 / (k.k)^eta},
		{"expanded", I (4 Pi)^(-2+eta) Gamma[1+eta] Beta[1-eta, 1-eta] / (eta (k.k)^eta)}
	},
	TestID->"I2(y,0)",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[1/((l-p).(l-p) (l-k).(l-k)), l];
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`T0 / (q.q)^eta,
	TestID->"I2(p,k) compact",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[1/(l.l (l-p).(l-p) (l-k).(l-k)), l];
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`R0 (k.k)^(-1-eta),
	TestID->"I3(p,k,0) compact",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[l.xx/(l.l (l-k).(l-k)), l]; 
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2+eta) Gamma[1+eta] Axiloop`Private`T1 k.xx / (2 (k.k)^eta),
	TestID->"I2x(y,0) compact",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[l.xx/((l-p).(l-p) (l-k).(l-k)), l]; 
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2 + eta) Gamma[1 + eta] (q.q)^-eta (Axiloop`Private`T0 k.xx + 1/2 Axiloop`Private`T1 (p.xx - k.xx)),
	TestID->"I2x(p,k) compact",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[l.xx/(l.l (l-p).(l-p) (l-k).(l-k)), l]; 
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (xx.p Axiloop`Private`R1 + xx.k Axiloop`Private`R2) (k.k)^(-1-eta),
	TestID->"I3x(p,k,0) compact",
	EquivalenceFunction->EqualSimplify
]


expr := IntegrateLoop[l.xx l.yy / (l.l (l-p).(l-p) (l-k).(l-k)), l]; 
Test[
	GetValue[expr, "compact"],
	I (4 Pi)^(-2+eta) Gamma[1+eta] (
				xx.p yy.p Axiloop`Private`R3 +
				xx.k yy.k Axiloop`Private`R4 +
				(xx.k yy.p + xx.p yy.k) Axiloop`Private`R5 +
				k.k xx.yy Axiloop`Private`R6
			) (k.k)^(-1-eta),
	TestID->"I3xy(p,k,0) compact",
	EquivalenceFunction->EqualSimplify
]