(* Axiloop Package for Wolfram Mathematica
 *
 * Author:   Oleksandr Gituliar <gituliar@gmail.com>
 * Created:  04 May 2012
 *
 * Copyright (c) 2012 Oleksandr Gituliar
 *)


BeginPackage["Axiloop`", {"Axiloop`Tracer`"}]

epsilon::usage =
	"Dimensional regulator; comes from trace operations."

eta::usage =
	"Dimensional regulator; comes from loop interals."

g::usage =
	"Quark-gluon coupling constant."

k::usage =
	"Outgoing particle momentum; k.n = x."

l::usage =
	"Loop momentum."

n::usage =
	"Light-cone gauge vector; n.n = 0."

p::usage =
	"Incoming particle momentum; p.n = 1."

q::usage = "Final state particle momentum; q.q = 0."

x::usage =
	"x = k.n"

DefineKernel::usage =
	"The definition of DGLAP evolution kernel."

ExtractPole::usage =
	"ExtractPole[expr, x] extract coefficient in front of 1/x in expr."

FPx::usage =
	"Crossed fermion propagator in light-cone gauge."
	
FP::usage =
	"Fermion propagator in light-cone gauge."

FV::usage =
	"Fermion vertex in light-cone gauge."
	
GPx::usage =
	"Crossed gluon propagator in light-cone gauge."
	
GP::usage =
	"Gluon propagator in light-cone gauge."

GV::usage =
	"Gluon vertex in light-cone gauge."

I0::usage =
	"Principal Value regulated integral; I0 = - Log[delta] + O[delta]."

I1::usage =
	"Principal Value regulated integral; I1 = - (Log[delta]^2)/2 - Li2[1]/4 + O[delta]."

IntegrateFinal::usage =
	"Integrate kernel over final particle momenta."

IntegrateLoop::usage =
	"Integrate kernel over internal loop momenta."

Kernel::usage =
	"Kernel constructor; define and integrate a kernel."

KernelGet::usage =
	"KernelGet[kernel_, key_] get value from kernel associated with key."

Li2::usage =
	"Dilogarythm function; Li2[x] = - Integrate[Log[1-t]/t, {t, 0, z}]."

Begin["`Private`"]

(* Useful modifications to standard functions *)

Unprotect[ReplaceRepeated];
	ReplaceRepeated[expr_,{{}}] := expr;
	ReplaceRepeated[expr_,{rules__List}] := ReplaceRepeated[expr //. First[{rules}], Rest[{rules}]];
Protect[ReplaceRepeated];

(* Kinematics definition and some transformations. *)

Unprotect[S];
  S[n,n] = 0;
  S[n,p] = S[p,n]; S[p,n] = 1;
  S[n,k] = S[k,n]; S[k,n] = x;
  S[n,q] = S[q,n]; S[q,n] = 1 - x;
  S[k,p] = S[p,k]; S[p,k] = (p.p + k.k - q.q) / 2;
  S[k,q] = S[q,k]; S[q,k] = (p.p - k.k - q.q) / 2;
  S[p,q] = S[q,p]; S[q,p] = (p.p - k.k + q.q) / 2;
  S[q,q] = 0;
Protect[S];

Spur[f0];

(* Set a space-time dimension where traces are calculated. *)

VectorDimension[4-2*epsilon];

(* Define Feynman rules for light-cone gauge. *)

fermionLines = {};

LorentzTensor[mu_, nu_, p_] :=
	- {mu}.{nu} + (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n;

FPx[p_, line_:f1] := (
	fermionLines = Union[fermionLines, {line}];
	I G[line, p]
);

FP[p_, line_:f1] := FPx[p, line] / p.p;

FV[mu_, line_:f1] := (
	fermionLines = Union[fermionLines, {line}];
	- I g G[line, {mu}]
);

GPx[mu_, nu_, p_] :=
	I LorentzTensor[mu, nu, p];

GP[mu_, nu_, p_] :=
	GPx[mu, nu, p] / p.p;

GV[i1_,p1_, i2_,p2_, i3_,p3_] :=
	g ( {i1}.{i2} (p1.{i3}-p2.{i3}) + {i2}.{i3} (p2.{i1}-p3.{i1}) + {i3}.{i1} (p3.{i2}-p1.{i2}) );

(* DGLAP evolution kernel according to CFP *)

DefineKernel[L_, M__, R_] := Module[{spurRules = (#->f0)& /@ fermionLines},
    x ExpandNumerator[(G[f1,n]/(4 k.n))**L**NonCommutativeMultiply@@M**R //. spurRules]
];

Kernel[L_, M__, R_, kernel_:0] := Module[{definition, exclusive, inclusive, Z},
	definition = DefineKernel[L, M, R];

	exclusive = IntegrateLoop[definition, l];

	PeZ = ExtractPole[exclusive, eta]  //. epsilon -> 0;
	Pe = KernelGet[kernel, "exclusive"];
	Z = PeZ / Pe //. epsilon -> 0;

	var01 = IntegrateFinal[exclusive - Pe Z / eta] //. eta -> epsilon;
	var02 = ExtractPole[var01, epsilon];
	inclusive = Collect[Expand[var02], {(Log[x])^2, Log[x] Log[1-x], x Log[x], I0 Log[x], I0 Log[1-x], Log[x], Log[1-x], I0, I1, Li2[1]}, Simplify];
 
	{
		{"definition", definition},
		{"exclusive", exclusive},
		{"inclusive", inclusive},
		{"Z", Z}
	}
];

KernelGet[kernel_, key_] := Last[First[Select[kernel, First[#] == key &]]];

IntegrateFinal[kernel_] := Module[{},
	(4 Pi)^(-2+epsilon) / Gamma[1-epsilon] (1-x)^(-epsilon) Integrate[(k.k)^(-epsilon) kernel, k.k]
];

(* IntegrateLoop and its helpers *)

CollectIntegralRules[l_] := {
	KK[l, {x___},{y___},{z___}] S[l,p_] :>
		KK[l, {p,x},{y},{z}],
	KK[l, {x___},{y___},{z___}] Power[S[l,p_], n_] :>
		KK[l, {p,x},{y},{z}] Power[S[l,p], n-1] /; n>0,

	KK[l, {x___},{y___},{z___}] / S[l,l] :>
		KK[l, {x},{0,y},{z}],
	KK[l, {x___},{y___},{z___}] / S[l-l1_,l-l1_] :>
		KK[l, {x},{l1,y},{z}],
	KK[l, {x___},{y___},{z___}] / S[l+l1_,l+l1_] :>
		KK[l, {x},{-l1,y},{z}],

	KK[l, {x___},{y___},{z___}] /  S[l,n] :>
		KK[l, {x},{y},{0,z}],
	KK[l, {x___},{y___},{z___}] / (S[l,n]-S[ln_,n]) :>
		KK[l, {x},{y},{ln,z}],
	KK[l, {x___},{y___},{z___}] / (S[ln_,n]-S[l,n]) :>
		- KK[l, {x},{y},{ln,z}],
	KK[l, {x___},{y___},{z___}] /  S[l+ln_,n] :>
		KK[l, {x},{y},{-ln,z}],

	KK[l, {},{},{}] -> 1
};

CollectIntegral[expr_, l_] := Expand[expr * KK[l, {},{},{}], l] //. CollectIntegralRules[l] ;

ReduceIntegralRules[l_] := {
	{
		KK[l, {x1___,l,x2___},{y1___,0,y2___},{z___}] :>
            KK[l, {x1,x2},{y1,y2},{z}],

		KK[l,{x1___,p_,x2___},{y1___,p_,y2___},{z___}] :>
			(KK[l, {x1,x2},{y1,p,y2},{z}] p.p + KK[l, {x1,l,x2},{y1,p,y2},{z}] - KK[l, {x1,x2},{y1,y2},{z}]) / 2,

		KK[l, {x___},{y___},{z1___,p_,k_,z2___}] :>
			(KK[l, {x},{y},{z1,p,z2}] - KK[l, {x},{y},{z1,k,z2}]) / (p.n-k.n),

		KK[l, {x1___,n,x2___},{y___},{z1___,p_,z2___}] :>
			KK[l, {x1,x2},{y},{z1,z2}] + KK[l, {x1,x2},{y},{z1,p,z2}] p.n
	}, {
		KK[l, {l},{y1___,p_,y2___},{z___}] :>
			KK[l, {},{y1,y2},{z}] + 2 KK[l, {p},{y1,p,y2},{z}] - p.p KK[l, {},{y1,p,y2},{z}],

		KK[l, {},{y___},{p_Symbol}] :>
			KK[l, {},(#-p)&/@{y},{0}],

		KK[l, {x_},{y___},{p_Symbol}] :>
			KK[l, {x},(#-p)&/@{y},{0}] + x.p KK[l, {},(#-p)&/@{y},{0}]
	}, {
		KK[l, {},{y_Symbol},{0}] :>
			KK[l, {},{0},{0}] + 2 KK[l, {y},{y,0},{0}] - y.y KK[l, {},{y,0},{0}]
	}
};

ReduceIntegral[expr_, l_] := Module[{},
	expr //. ReduceIntegralRules[l]
]

IntegrateLoopRules[l_] := {
						K[{},{p},{}] -> K[{},{0},{}],
						K[{},{k},{}] -> K[{},{0},{}],

(* K1(0; 0)       *)	K[{},{0},{0}] -> 0,

(* K2(k,0; 0)     *)	K[{},{k,0},{0}]   -> Q (k.k)^(-eta) P0/x,
(* K2(p,0; 0)     *)	K[{},{p,0},{0}]   -> Q (p.p)^(-eta) B0,
(* K2(p,k; 0)     *)	K[{},{p,k},{0}]   -> 0,
(* K3(p,k,0; 0)   *)	K[{},{p,k,0},{0}] -> Q (k.k)^(-1-eta) S0,

(*                *)	K[{xx_},{0,y_},{0}]  :> K[{xx},{y,0},{0}],
(* K2x(k,0; 0)    *)	K[{xx_},{k,0},{0}]   :> Q (k.k)^(-eta) / k.n (xx.k P1 + xx.n k.k/(2 k.n) P3 ),
(* K2x(p,0; 0)    *)	K[{xx_},{p,0},{0}]   :> Q (p.p)^(-eta) / p.n (xx.p B1 + xx.n p.p/(2 p.n) B3 ),
(* K3x(p,k,0; 0)  *)	K[{xx_},{p,k,0},{0}] :> Q (k.k)^(-1-eta) (xx.p S1 + xx.k S2 + xx.n k.k/(2 k.n) S3),

(* I2(y,0)        *)	K[{},{y_,0},{}]   :> Q (y.y)^(-eta) T0,
(* I2(p,k)        *)	K[{},{p,k},{}]    :> Q (q.q)^(-eta) T0,
(* I3(p,k,0)      *)	K[{},{p,k,0},{}]  :> Q (k.k)^(-1-eta) R0,

(*                *)	K[{xx_},{0,y_},{}]   :> K[{xx},{y,0},{}],
(* I2x(y,0)       *)	K[{xx_},{y_,0},{}]   :> Q (y.y)^(-eta) xx.y T0/2,
(* I2x(p,k)       *)	K[{xx_},{p,k},{}]    :> Q (q.q)^(-eta) (xx.p - xx.k) T0/2 + xx.k K[{},{p,k},{}],
(* I3x(p,k,0)     *)	K[{xx_},{p,k,0},{}]  :> Q (k.k)^(-1-eta) (xx.p R1 + xx.k R2),

(* I3xy(p,k,0)    *)	K[{xx_, yy_},{p,k,0},{}]  :> Q (k.k)^(-1-eta) (xx.p yy.p R3 + xx.k yy.k R4 + (xx.k yy.p + xx.p yy.k) R5 + k.k xx.yy R6 ),
                        Q -> I (4 Pi)^(-2+eta) Gamma[1+eta]
}

IntegrateLoopExpandRules = {
	B0 -> I0 / eta - I1 + Li2[1],
	B1 -> 1/eta Beta[1-eta, 1-eta],
	B3 -> (2 - I0) / eta + 4 + I1 - Li2[1],
	P0 -> (Log[x] + I0) / eta - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1],
	P1 -> T0,
	P3 -> (2 - Log[x] - I0) / eta + 4 + I1 - I0 Log[x] - (Log[x]^2)/2 - Li2[1],
	R0 -> 1/eta^2 - Li2[1],
	R1 -> 1/eta^2 + 2/eta + 4 - Li2[1],
	R2 -> -1/eta - 2,
	R3 -> R1 + 1/eta + 3,
	R4 -> -1/(2 eta) - 1,
	R5 -> -1/(2 eta) - 3/2,
	R6 -> 1/(4 eta) + 3/4,
	S0 -> 1/eta^2 + (Log[x] - I0)/eta + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x] - (Log[x]^2)/2,
	S1 -> 1/eta^2 - 1/eta Log[x] x/(1-x)  + x/(1-x) Li2[1-x] - Li2[1],
	S2 -> 1/eta Log[x]/(1-x) - Li2[1-x]/(1-x),
	S3 -> 1/eta (I0 + Log[x]/(1-x)) - I1 + I0 Log[x]/(1-x) - Li2[1] - x/(1-x) Li2[1-x] + (Log[x]^2)/2,
	T0 -> 1/eta Beta[1-eta, 1-eta]
}

IntegrateLoop[kernel_, l_, expand_:True] := Module[{step01, step02, step03},
	step01 = ReduceIntegral[CollectIntegral[kernel, l], l] //. KK[l, xyz___] -> K[xyz];
	step02 = step01 //. {{IntegrateLoopRules[l]}, {p.p -> 0}};
	step03 = If[expand == True, step02 //. IntegrateLoopExpandRules, step02];
	Simplify[ step03 //. {0^-eta -> 0, 0^(1-eta) -> 0, 0^(2-eta) -> 0} ]
];

(* Renormalization routines and helpers *)

ExtractPole[kernel_, eta_] := Simplify[Coefficient[Series[kernel, {eta, 0, 1}], eta, -1]];

End[]


EndPackage[]
