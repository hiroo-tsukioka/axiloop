(*========================= FILE: Tracer-1.1.1.m =========================*)
(*                                                                        *)
(*                              Last Change:                              *)
(*                      Mon Dec 30 15:36:00 MET 1991                      *)
(*                                                                        *)
(*                             "T R A C E R"                              *)
(*                        A MATHEMATICA PACKAGE FOR                       *)
(*                 GAMMA-ALGEBRA IN ARBITRARY DIMENSIONS                  *)
(*                   (based on MATHEMATICA Version 1.2)                   *)
(*                                                                        *)
(*    Authors: Matthias Jamin and Markus E. Lautenbacher,                 *)
(*             Physics Department, Theoretical Physics T31,               *)
(*             Technical University Munich,                               *)
(*             D-8046 Garching, FRG.                                      *)
(*             INTERNET-email:                                            *)
(*                 jamin@feynman.t30.physik.tu-muenchen.de                *)
(*                lauten@feynman.t30.physik.tu-muenchen.de                *)
(*                                                                        *)
(*             Bug reports (hopefully few) and suggestions welcome !      *)
(*                                                                        *)
(* References: M. Veltman, Nucl.Phys. B319(89), 253;                      *)
(*             P. Breitenlohner, D. Maison, Comm.Math.Phys. 52(77), 11;   *)
(*             G. Rufa, Annalen der Physik 47(90), 6.                     *)
(*                                                                        *)
(*========================================================================*)
 
(**************************************************************************)
(*                                                                        *)
(*                 COPYRIGHT AND DISCLAIMER OF WARRANTY                   *)
(*                                                                        *)
(*     Copyright (C) 1991 Markus E. Lautenbacher and Matthias Jamin.      *)
(*     This file is part of the TRACER package based on MATHEMATICA       *)
(*     version 1.2.                                                       *)
(*                                                                        *)
(*     The TRACER package is distributed in the hope that it will be      *)
(*     useful, but WITHOUT ANY WARRANTY.  No author or distributor        *)
(*     accepts responsibility to anyone for the consequences of using     *)
(*     it or for whether it serves any particular purpose or works        *)
(*     at all, unless he says so in writing.                              *)
(*                                                                        *)
(*     Permission is granted for use and non-profit distribution of       *)
(*     this file providing that this notice be clearly maintained, but    *)
(*     the right to distribute this file for profit or as part of any     *)
(*     commercial product is specifically reserved for the authors.       *)
(*     The same permission and reservation applies to the formatted       *)
(*     document produced from this file.                                  *)
(*                                                                        *)
(*     If you find the TRACER package useful for doing research,          *)
(*     acknowledging the authors' work by proper reference to the         *)
(*     package would be appreciated.                                      *)
(*                                                                        *)
(*     Please do not change this file. If you have to, document the       *)
(*     changes you have made !                                            *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(*     Changes made by O.Gituliar <gituliar@gmail.com>:                   *)
(*                                                                        *)
(*       * switch from Tracer` to Axiloop`Tracer` context;                *)
(*       * rename GammaTrace -> GTrace;                                   *)
(*       * remove `G = GTrace` alias;                                     *)
(*                                                                        *)
(**************************************************************************)
 
(*------------------------ BEGIN CONTEXT TRACER --------------------------*)
 
BeginPackage[ "Axiloop`Tracer`" ];
 
 
(*---------------------- UNPROTECT SPECICAL SYMBOLS ----------------------*)
(*------------------------ AND MAKE "TABULA RASA" ------------------------*)
 
Unprotect[ AntiCommute, ContractEpsGamma, Eps, GTrace, G5, H,
           ListCommands, NoSpur, OnShell, OutputFormat, RemoveHatMomenta,
           RemoveNCM, S, Sigma, SortLine, Spur, T, ToDiracBasis,
           ToHatTilde, ToOtimes, ToUG5, U, VectorDimension, Version ];
 
Remove[ "Axiloop`Tracer`*", "Axiloop`Tracer`Private`*" ];
 
 
(*-------------------------- STATUS OF TRACER -----------------------------*)
 
Authors = "
Matthias Jamin and Markus E. Lautenbacher,\n
Physics Department, Theoretical Physics T31,\n
Technical University Munich,\n
D-8046 Garching, FRG.\n
INTERNET-email:  jamin@feynman.t30.physik.tu-muenchen.de\n
                lauten@feynman.t30.physik.tu-muenchen.de";
 
MathVersion   = "1.2";
TracerVersion = "1.1.1";
LastChange    = "Mon Dec 30 15:36:00 MET 1991";
(* if this list grows to long MATHEMATICA gets an error on start-up *)
MajorChanges = "
- Rules for NonCommutativeMultiply[] moved to the end of Tracer";
 
Commands = "The package defines the following commands:
\n\"AntiCommute\", \"ContractEpsGamma\", \"Eps\", \"G\",
\"GTrace\", \"G5\", \"H\",\n\"ListCommands\", \"NoSpur\",
\"OnShell\", \"OutputFormat\", \"RemoveHatMomenta\",\n
\"RemoveNCM\", \"S\", \"Sigma\", \"SortLine\", \"Spur\", \"T\",
\"ToDiracBasis\",\n\"ToHatTilde\", \"ToOtimes\", \"ToUG5\", \"U\",
\"VectorDimension\", \"Version\".
\n                                    Help on usage as usual per ?Name.\n";
 
 
(*--------------------------- STARTUP MESSAGE -----------------------------*)
 
Print[" \n                               T R A C E R"];
Print["                              =============\n \n"];
Print["     A MATHEMATICA PACKAGE FOR GAMMA-ALGEBRA IN ARBITRARY DIMENSIONS"]
Print["                     by M. Jamin and M.E. Lautenbacher"];
Print["              Physics Dept. T31, Technical University Munich\n \n"];
Print["             Version ", TracerVersion, " from ", LastChange];
Print["                    (based on MATHEMATICA Version ", MathVersion,
      ")\n \n"];
Print[ Commands ];
Print["DEFAULT SETTINGS ON STARTUP:
\n----------------------------" ];
 
 
(*------------------ SET UP SYMBOLS OF PACKAGE TO EXPORT -----------------*)
 
AntiCommute::usage =
"AntiCommute[flag]: Depending on flag (on/off) toggle the usage of an
(non-)anticommuting matrix gamma_5.
\nAntiCommute[]: Show the currently used type of G5.";
 
ContractEpsGamma::usage =
"ContractEpsGamma[expr] contracts index pairs appearing in \"expr\"
according to the formula
\n    eps_{mu nu la rho} gamma^rho =
\n    i gamma_5 (  g~_{mu nu} gamma~_la - g~_{mu la} gamma~_nu
\n               + g~_{nu la} gamma~_mu - gamma~_mu gamma~_nu gamma~_la ),
\n   where as usual the tilde symbol denotes 4-dimensional objects.";
 
Eps::usage =
"Eps[arg1,arg2,arg3,arg4] is the completely antisymmetric product of
the four arguments \"arg1-4\". Eps[] is multi-linear with respect to
a linear combination with scalar coefficients. Arguments of Eps[]
can be a mixture of Lorentz indices and momenta, e.g.
\"Eps[[p,{mu},q,{nu}]\" means \"Eps_{alpha mu beta nu} p^alpha q^beta\"
in a standard physics notation.";

GTrace::usage =
"GTrace[l,exp1,...,expn] calculates the trace of a string of gamma
matrices in arbitrary dimensions. The space-time dimension can be set with
\"VectorDimension[]\". The expi can be: Symbols ( e.g. \"p\", \"q\" ) which
denote vectors, brackets (e.g. \"{mu}\", \"{nu}\" ) which denote indices or
expressions like \"p+m U+a G5\" where U is the unit matrix, G5 is gamma5 and
m and a are scalar coefficients. Indices which occur in pairs are contracted
automatically. Different strings are distinguished by the line index l.";
 
G5::usage =
"G5 is the matrix gamma_5 in d dimensional Gamma algebra.";
 
H::usage =
"H@Symbol is the projection of the d dimensional object \"Symbol\" onto
(d-4) dimensions.
\nDisplay format: H@p --> p^";
 
ListCommands::usage = 
"ListCommands[] shows a list of the commands defined by the Tracer
package.";
 
NoSpur::usage =
"NoSpur[l1, ..., ln] removes the gamma lines \"l1\", ..., \"ln\" from
being traced.";
 
OnShell::usage =
"OnShell[flag, list1, ..., listn]: Depending on flag (on/off), the
on-shell declarations specified in the \"listi\" are enforced or
removed.\n
Example: OnShell[on, {p1,m^2}, {s1,p1,0}, {e,-1}] declares the
substitutions S[p1,p1]=m^2, S[s1,p1]=0, S[e,e]=-1.
\nOnShell[]: Show all currently active on-shell declarations.";
 
OutputFormat::usage = 
"OutputFormat[flag]: Depending on flag (texlike/subscript) toggle the output
format of the scalar product \"S[expr]\" between TeX-input-like and
subscripted style.
\nOutputFormat[]: Show the currently used output format for \"S[expr]\".
\nFor examples of the display formats see the help entry on \"S\".";
 
RemoveHatMomenta::usage = 
"RemoveHatMomenta[expr, p1, p2, ...] sets all \"d-4\" dimensional momenta
\"p1^\", \"p2^\", ... in \"expr\" equal to 0.";
 
RemoveNCM::usage =
"RemoveNCM[flag]: Depending on flag (on/off) toggle whether
NonCommutativeMultiply[] should be removed or kept if all gamma lines
are different.
\nRemoveNCM[]: Show current treatment of NonCommutativeMultiply[].";
 
S::usage =
"S[arg1,arg2] is the scalar product of the two arguments. S is
symmetric and bilinear with respect to a linear combination with scalar
coefficients.  Use of the Dot operator for the scalar product is
possible.
\nDisplay formats:
\n          Input:                       OutputFormat[flag]:
\n(explicit)      (shorthand)          texlike      subscript
\nS[p, q]         p.q           -->    p.q          p.q
\nS[p,{mu}]       p.{mu}        -->    p_{mu}       p
\n                                                   mu
\nS[{mu},{nu}]    {mu}.{nu}     -->    g_{mu nu}    g
\n                                                   mu nu";
 
Sigma::usage =
"\"Sigma[ l,p,q ]\" is a shorthand notation for
\"I/2(G[l,p,q]-G[l,q,p])\".  It is decomposed immediately into the
equivalent gamma matrix strings.  In the output of \"ToDiracBasis[]\"
this automatic decomposition is prevented by an internal application of
the Hold[] command invisible to the user.  Thus, to use any \"Sigma[]\"
stemming from the output of \"ToDiracBasis[]\" in further calculations
you first have to apply a \"Release[]\" on the output.";
 
SortLine::usage =
"SortLine[ expr, l, reflist ] sorts the product of gamma matrices on line
`l' in expression `expr' with respect to the reference list `reflist'. 
If `l' does not match exactly one gamma matrix line in expr, SortLine[]
returns \"expr\" unchanged.\n
Example: SortLine[ G[l1,p,{m},{n},q] G[l2,p,q], l1, {{m},p,{n},q}]\n
	   --> ( 2 p_{m} G[l1,{n},q] - G[l1,{m},p,{n},q] ) G[l2,p,q]";
 
Spur::usage =
"Spur[l1, ..., ln]
adds the gamma-matrix lines with line indices \"l1\", ..., \"ln\" to
the set of lines to be traced.
\nSpur[] shows the currently used gamma lines which should be traced.";
 
T::usage =
"T@Symbol is the projection of the d dimensional object \"Symbol\" onto
4 dimensions.
\nDisplay format: T@p --> p~";
 
ToDiracBasis::usage =
"\"ToDiracBasis[expr]\" decomposes an expression containing strings
\"G[...]\" of gamma matrices into the basis spanned by \"1\", \"g_5\",
\"g_mu\", \"g_mu g_5\" and \"I/2 [g_mu,g_nu]\" for all fermion lines
appearing in \"expr\". Since this basis is valid for space-time
dimensions equal to \"4\" only, the routine tests whether
\"VectorDimension[4]\" has been set and if not issues a warning
and exits.\n
\"ToDiracBasis[expr,{l1,l2,...}]\" decomposes only fermion lines
\"l1\", \"l2\", ... in \"expr\".\n
In the output of \"ToDiracBasis[]\" the automatic decomposition of
\"Sigma[]\" to gamma strings is prevented by an internal application of
the Hold[] command invisible to the user.  Thus, to use any \"Sigma[]\"
stemming from the output of \"ToDiracBasis[]\" in further calculations
you first have to apply a \"Release[]\" on the output.\n
Note that applying \"ExpandAll[]\" on expressions containing held
\"Sigma[]\" terms leads to weird output. Use commands
\"ExpandNumerator[]\" and \"ExpandDenominator[]\" instead.";
 
ToHatTilde::usage = 
"ToHatTilde[expr] decomposes \"d\" dimensional gamma algebra objects
according to the formula \"gamma_mu = gamma~_mu + gamma^_mu\" into
\"4\" and \"d-4\" dimensional objects, respectively. \"4\" dimensional
objects which's default redecomposition into \"d\" and \"d-4\" ones is
delayed via the \"Hold[T[]]\" command are displayed with parentheses
around them, e.g. \"Hold[T@{mu}]\" --> \"({mu}~)\". To go back to the
\"d\" and \"d-4\" objects just do a \"Release[]\" on the expression
containing \"4\" dimensional objects.\n
Note that applying \"ExpandAll[]\" on expressions containing terms like
\"Hold[T@{mu}]\" leads to weird output. Use commands
\"ExpandNumerator[]\" and \"ExpandDenominator[]\" instead.";
 
ToOtimes::usage =
"\"ToOtimes[expr]\" transforms all products of the form
Times[G[...],G[...],\n ...]\" to \"Otimes[G[...],G[...],...]\"
preserving the order of the \"G[...]\"'s. It is most usefully used as
\"expr // ToOtimes // TeXForm\" acting as a filter to give output of
\"expr\" in \"TeXForm\" with the \"\\otimes\" symbol between
\"G[...]\"'s stemming from different lines. Note that
\"ToOtimes[expr]\" is needed in \"G[...] G[...] // TeXForm\" but not in
\"G[...]**G[...] // TeXForm\" since the built-in \"TeXForm\" of \"**\"
is already \"\\otimes\".";
 
ToUG5::usage =
"ToUG5[expr] decompos all matching fermion lines appearing in \"expr\"
according to the formula
\n    a G[l,strg1,strg2] + b G[l,strg1,G5,strg2] =
\n    (a+b)/2 G[l,strg1,U+G5,strg2] + (a-b)/2 G[l,strg1,U-G5,strg2],
\n   with a!=0 && b!=0.\n
ToUG5[expr,{l1,l2,...}] decomposes only fermion lines \"l1\", \"l2\",
etc.. in \"expr\" into \"1 -+ gamma_5\" terms.";
 
U::usage = 
"U is the d dimensional unit matrix. For instance U is used in 
connection with mass terms as \"G[l, p+m U]\".";
 
VectorDimension::usage =
"VectorDimension[expr]: Set the space-time dimension to \"expr\". The
default symbol for the dimension is \"Global`d\". Do not use the symbol
\"D\" for the dimension since this may mix up with the differential
operator \"System`D\".
 
\nVectorDimension[]: Show the currently used expression for the space-time 
dimension.";
 
Version::usage = 
"Version[] prints out some information on the current version of the
TRACER package.";
 
 
(*------------------ BEGIN PRIVATE CONTEXT OF PACKAGE --------------------*)
 
Begin[ "`Private`" ];                   (* Hide actual code from the user *)
 
 
(*--------------------- PRINT INFO ON CURRENT VERSION  -------------------*)
 
ListCommands[] := ( Print[]; Print[ Commands ] );
 
Version[] :=
   (
     Print[];
     Print["AUTHORS: ", Authors];
     Print[];
     Print["VERSION:       ", TracerVersion, " based on Mathematica ", 
           MathVersion]; 
     Print["LAST CHANGE:   ", LastChange];
     Print["MAJOR CHANGES:"];
     Print[MajorChanges]
   );
 
 
(*--------------------------- OUTPUT FORMATS -----------------------------*)
 
(* Hat and tilde forms *)
 
Format[H[p_]] := StringJoin[ ToString[p], "^" ];
 
Format[T[p_]] := StringJoin[ ToString[p], "~" ];
 
(* Scalar product S[p,q] or p.q *)
 
StoString[p_,q_]:=                          (*  also used by OnShell  *)
  If[ TeXlike,
   Switch[ Head[p] Head[q],
	       List List, StringJoin[ "g_{", ToString[p[[1]]], " ",
				      ToString[q[[1]]], "}" ],
	     Symbol List, StringJoin[ ToString[p], "_",  ToString[q] ],
	       Hold Hold, Switch[ Head[p[[1,1]]] Head[q[[1,1]]],
	                    List List, StringJoin["(g~_{",ToString[p[[1,1,1]]],
	                                  " ", ToString[q[[1,1,1]]], "})" ],
	                  Symbol List, StringJoin[ "(", ToString[p[[1,1]]],
	                                        "~_", ToString[q[[1,1]]],")" ],
	                            _, StringJoin[ "(", ToString[p[[1,1]]],
	                                       "~.", ToString[q[[1,1]]], "~)" ]
                              ],
		         _, StringJoin[ ToString[p], ".", ToString[q] ]
	   ],
   Switch[ Head[p] Head[q],
	       List List, Subscripted[ "g"[ p[[1]] q[[1]] ] ],
	     Symbol List, Subscripted[ ToString[p][ q[[1]] ] ],
	       Hold Hold, Switch[ Head[p[[1,1]]] Head[q[[1,1]]],
	                    List List,
	                         Subscripted[ "(g~)"[p[[1,1,1]] q[[1,1,1]]] ],
	                  Symbol List, Subscripted[ StringJoin[ "(",
	                         ToString[p[[1,1]]], "~)"][ q[[1,1,1]] ] ],
	                            _, StringJoin[ "(", ToString[p[[1,1]]],
	                                       "~.", ToString[q[[1,1]]], "~)" ]
                              ],
		       _, StringJoin[ ToString[p], ".", ToString[q] ]
	   ]
    ];
 
Format[S[p_,q_]] := StoString[p,q];
 
Format[H[p_,q_]]:= 
  If[ TeXlike,
   Switch[ Head[p] Head[q],
	     List List, StringJoin[ "g^_{", ToString[p[[1]]], " ",
				      ToString[q[[1]]], "}" ],
	   Symbol List, StringJoin[ ToString[p],"^_",ToString[q] ],
		     _, StringJoin[ ToString[p],"^.",ToString[q],"^" ]
	   ],
   Switch[ Head[p] Head[q],
	       List List, Subscripted[ "g^"[ p[[1]] q[[1]] ] ],
	     Symbol List, Subscripted[ StringJoin[ ToString[p], "^" ]
				       [ q[[1]] ] ],
		     _, StringJoin[ ToString[p],"^.",ToString[q],"^" ]
	   ]
    ];
 
(* Eps-tensor as Eps_{p1,p2,p3,p4} *)
 
Format[eps[{ p1_,p2_,p3_,p4_ }]]:=
   StringJoin[ "Eps[", ToString[p1], ",", ToString[p2],
		     ",", ToString[p3], ",", ToString[p4], "]" ];
 
(* Unevaluated parts of tr[l,{list}] *)
 
Format[tr[l_,{} ]] := StringJoin[ "U[ ", ToString[ l ], " ]" ];
 
Format[tr[l_,{ a__ } ]] :=
  Block[ {i, string = StringJoin[ "G[ ", ToString[ l ] ]},
          Do[
             Switch[ Head[ {a}[[i]] ],
                     List, string = StringJoin[ string, ", {",
                                       ToString[ {a}[[i,1]] ], "}" ],
                     Hold, string = StringJoin[ string, ", (",
                                       ToString[ {a}[[i,1]] ], ")" ],
                        _, string = StringJoin[ string, ", ", 
                                                ToString[ {a}[[i]] ] ]
                   ]
             ,{ i, Length[{a}] }
            ];
          string = StringJoin[ string, " ]" ];
          string
        ];
 
(* Format for sigma[l, Hold[]] produced by ToDiracBasis *)
 
Format[ sigma[l_, Hold[{m_,n_}]] ] :=
  StringJoin[ "Sigma[ ",ToString[l],",",ToString[m],",",ToString[n]," ]"];
 
 
(*------------ TeXForm FOR SCALAR PRODUCT AND EPSILON TENSOR  ------------*)
 
(* Symbols in (4-d) and 4 dimensions *)
 
Format[H[p_], TeXForm] := 
   Switch[ Head[p],
	     List, StringJoin["\\hat\\gamma_{", ToString[TeXForm@p[[1]]],"}"],
	     Symbol, StringJoin["\\not\\!\\hat ", ToString[TeXForm@p] ]
	   ];
 
Format[T[p_], TeXForm] := 
   Switch[ Head[p],
	     List, StringJoin["\\tilde\\gamma_{",ToString[TeXForm@p[[1]]],"}"],
	     Symbol, StringJoin["\\not\\!\\tilde ", ToString[TeXForm@p] ]
	   ];
 
(* Scalar product in d dimensions *)
 
Format[S[p_,q_], TeXForm]:= 
   Switch[ Head[p] Head[q],
	       List List, StringJoin[ "g_{", ToString[TeXForm@p[[1]]], " ", 
				      ToString[TeXForm@q[[1]]], "}" ],
	     Symbol List, StringJoin[ ToString[TeXForm@p], "_{",  
				      ToString[TeXForm@q[[1]]], "}" ],
		       _, If[ p === q,
			        StringJoin[ ToString[TeXForm[p]], "^{2}" ],
			        StringJoin[ ToString[TeXForm@p]," \\cdot ",  
				        ToString[TeXForm@q] ]
			      ]
	   ];
 
(* Scalar product in (4-d) dimensions *)
 
Format[H[p_,q_], TeXForm]:= 
   Switch[ Head[p] Head[q],
	     List List, StringJoin[ "\\hat g_{", ToString[TeXForm@p[[1]]],
				    ToString[TeXForm@q[[1]]], "}" ],
	   Symbol List, StringJoin[ "\\hat ", ToString[TeXForm@p], "_{",  
				    ToString[TeXForm@q[[1]]], "}" ], 
		     _, If[ p === q,
			      StringJoin["\\hat ",ToString[TeXForm[p]],"^{2}"],
			      StringJoin[ "\\hat ", ToString[TeXForm@p], 
                                 " \\cdot \\hat ", ToString[TeXForm@q] ]
			  ]
	   ];
 
(* Epsilon tensor *)
 
Format[eps[{p1_,p2_,p3_,p4_}], TeXForm]:=
   StringJoin[ "\\epsilon\\left(", 
		 ToString[TeXForm@p1], ",", ToString[TeXForm@p2], ",", 
		 ToString[TeXForm@p3], ",", ToString[TeXForm@p4],
		 "\\right)" 
	       ];
 
(* Unevaluated parts of tr[l,{list}]; the "Plus" in Switch is needed
   for "ToUG5 *)
 
Format[ G5, TeXForm ] = "\\gamma_{5}";
 
Format[ U, TeXForm] = 1;
 
Format[tr[l_,{} ], TeXForm] = 1;
 
Format[tr[l_,{ a__ } ], TeXForm] :=
  Block[ {i, TeXstring=""},
	    Do[
	       Switch[ {a}[[i]],
		  _List, TeXstring = StringJoin[ TeXstring, " \\gamma_{",
					ToString[TeXForm@{a}[[i,1]]],"} "],
   Hold[T[_List]], TeXstring = StringJoin[ TeXstring, " \\tilde\\gamma_{",
					ToString[TeXForm@{a}[[i,1,1,1]]],"} "],
 Hold[T[_Symbol]], TeXstring = StringJoin[ TeXstring, " \\tilde{\\not\\! ",
					ToString[TeXForm@{a}[[i,1,1]]],"} "],
		_Symbol, TeXstring = StringJoin[ TeXstring,
				       If[ {a}[[i]]===G5 || {a}[[i]]===U,
					   "", " \\not\\! "],
					ToString[TeXForm@{a}[[i]]]," "],
		  _Plus, TeXstring =
			 StringJoin[ TeXstring, " ",
				     Switch[ { {a}[[i,1]],{a}[[i,2]] },
					     { G5,U}, "(1+\\gamma_{5})",
					     {-G5,U}, "(1-\\gamma_{5})",
					     _, ToString[TeXForm@{a}[[i]]]
					   ], " "
				   ],
		      _, TeXstring = StringJoin[ TeXstring, " ",
					ToString[TeXForm@{a}[[i]]]," "] ],
	       { i, Length[{a}] }
	      ];
	    TeXstring
	  ];
 
 
(* Format for sigma[l, Hold[]] produced by ToDiracBasis *)
 
Format[ sigma[l_, Hold[{m_,n_}]], TeXForm ] :=
  Switch[ Head[m] Head[n],
            List List, StringJoin[ "\\sigma_{", ToString[TeXForm@m[[1]]],
                                 " ", ToString[TeXForm@n[[1]]], "}" ],
          Symbol List, StringJoin[ "{i\\over 2} [", ToString[TeXForm@m],
                           ",\\gamma_{", ToString[TeXForm@n[[1]]], "}]" ],
                    _, StringJoin[ "{i\\over 2} [", ToString[TeXForm@m],
                           ", ", ToString[TeXForm@n], "]" ]
        ];
 
 
(*------------------------------------------------------------------------*)
(*---------------------- BEGINNING Of THE MAIN RULES ---------------------*)
(*------------------------------------------------------------------------*)
 
(*--------------------- CONVERSION TO INTERNAL FORMAT --------------------*)
 
(* 
 * Convert user level functions G, GTrace, Eps, Sigma to internal
 * functions tr, eps, sigma. All actual calculations are done with tr,
 * eps and sigma. Gamma algebra objects are combined in a list.
 *)
 
GTrace[line_, a___] := tr[line, {a}];
 
Sigma[l_,m_,n_] := sigma[l,{m,n}];
 
Eps[a_, b_, c_, d_] := eps[{a, b, c, d}];
 
 
(*-------------- USE DOT AS INPUT FOR THE SCALAR PRODUCT -----------------*)
 
Unprotect[ Dot ];
 
Dot[ a_, b_] := S[ a, b]   /;   ( !Length[a] > 1 || Head[a] === Plus ) &&
                                ( !Length[b] > 1 || Head[b] === Plus );
 
Protect[ Dot ];
 
 
(*------------------- TOGGLE USE OF ANTICOMMUTING G5 ---------------------*)
(* 
 * The rules anticommute G5 to the right and in case use {G5,G} = 2 G^ G5.
 * "G5flag" allows for control over the usage of the rules in "SortLine" 
 * and "ToUG5".    
 *)
 
AntiCommute[ flag_:query ] := 
   ( Switch[ flag,
	  Tracer`on, 
	     ( tr[l_,{a___,G5,b_,c___}] := -tr[l,{a,b,G5,c}] /; G5flag;
	       AntiComm := True ),
	  Tracer`off, 
	     ( Unprotect[H];
            H[__] := 0;
            H[__] =. ;
            Protect[H]; 
            tr[l_,{a___,G5,b_,c___}] := -tr[l,{a,b,G5,c}] +
	       2 tr[l,{a,H@b,G5,c}] /; G5flag;
	       AntiComm := False ) ];
     If[ AntiComm, 
	   Print[ "Package uses the usual anticommuting G5 in \"", 
		  ToString[d], "\" dimensions." ],
	   Print[ "Package uses a non anticommuting G5 in \"", ToString[d], 
		  "\" dimensions." ] ];
     If[ AntiComm && d =!= 4,
        Print[ StringJoin[ "WARNING: Use of the Epsilon tensor in d != 4 ",
             "dimensions with\n         anticommuting G5 is algebraically ",
             "inconsistent." ] ] ];
     G5flag := True
   );
 
 
(*----------------------- SET SPACE-TIME DIMENSION -----------------------*)
 
VectorDimension[ dim_:query ] :=
   Switch[ dim,
	   query, Print[ "Current dimension is \"", ToString[d], "\"." ],
	       4, ( Unprotect[ Release[protected] ];
		    d = 4; 
		    H[__] := 0;
		    protected = Protect[ H ];
		    Print["Dimension set to \"4\". NOTE: For this setting of",
			  " the dimension the" ];
		    AntiCommute[ on ]
		  ),
	       _, ( Unprotect[ Release[protected] ]; 
		    d = dim;
		    H[__] := 0;   (* Avoid error msg on multiple calls. *)
		    H[__] =.;
		    protected = Switch[ Head[d], 
					Symbol, Protect[H, Release[d]],
					     _, Protect[ H ]            ];
		    Print[ "Dimension set to \"", ToString[d], 
			   "\". Used type of G5 left unchanged." ]
		  )
	   ];
 
d = Global`d;         (* Set default symbol for the space-time dimension *)
protected = Join[ {H}, Protect[Release[d]] ];
 
 
(*----------------- MODIFY TABLE OF LINES TO BE TRACED -------------------*)
 
CalcTable = {};             (* default for lookup table of lines to trace *)
 
(*
 * if Spur[] and NoSpur[] are modified, also see
 * for a possible modification in ToDiracBasis[]
 *)
 
Spur[lines___] := ( CalcTable = Union[CalcTable, {lines}]; SpurOut[] );
 
NoSpur[lines___] := ( CalcTable = Complement[CalcTable, {lines}]; 
			SpurOut[] );
 
SpurOut[] := Block[ {i, string = "The gamma matrix line(s) \""},
	       Do[ string = StringJoin[ string,
			    ToString[ CalcTable[[i]] ], ", " ],
		   {i, Length[ CalcTable ] - 1} ];
	       string = StringJoin[ string, ToString[
		   CalcTable[[Length[CalcTable]]] ], "\" will be traced." ];
	       If[ Length[CalcTable]==0, string =
		   "No gamma matrix line will be traced.", ];
	       Print[ string ] ];
 
 
(*---------------- SET OUTPUT STYLE FOR GTrace[list] -----------------*)
 
OutputFormat[ flag_:query ] := 
   ( Switch[ flag,
	       Global`texlike,   TeXlike := True;,
	       Global`subscript, TeXlike := False; ];
     Print[ "Current OutputFormat is set to ",
	      If[ TeXlike, "\"texlike\"", "\"subscript\"" ], "." ]
   );
 
 
(*------------ TOGGLE TREATMENT OF NONCOMMUTATIVEMULTIPLY ----------------*)
 
RemoveNCM[ flag_:query ] :=
   ( Switch[ flag,
             Tracer`on,   NCMflag = True;,
             Tracer`off,  NCMflag = False; ];
     Print[ "NonCommutativeMultiply will be ",
            If[ NCMflag, "removed.", "kept." ] ]
   );
 
NCMflag = False;
 
 
(*----------------------- REMOVAL OF ALL TILDE PARTS ---------------------*)
 
(* Implementation of G~ = G - G^ *)
 
tr[l_,{a___, T@b_, c___}] := tr[l,{a, b, c}] - tr[l,{a, H@b, c}];
 
 
(*------------- REDUCTION OF TERMS WITH ONLY 'p' AND 'G5' FACTORS --------*)
 
(* 
 * Initalize flag for U-+G5 expansion. "ApartUG5" is toggled later on by 
 * "ToUG5" to allow for collection of terms with "U-+G5" structure.
 *)
 
ApartUG5 := True;
 
tr[l_,{a___, 0, b___}] := 0;
 
tr[l_,{a___, m_. U, b___}] := m tr[l,{a, b}];
 
tr[l_,{a___, p_+m_. U,b___}] := tr[l,{a,p,b}] + m tr[l,{a,b}] /; ApartUG5;
 
tr[l_,{a___, m_ G5, b___}] := m tr[l,{a, G5, b}];
 
tr[l_,{a___,p_+m_. G5,b___}]:=tr[l,{a,p,b}]+m tr[l,{a,G5,b}] /; ApartUG5;
 
 
(*-------------- DECOMPOSE SIGMA MATRIX INTO GAMMA STRINGS ---------------*)
 
(* trivial stuff *)
sigma[l_,{0,n_}] = 0;
sigma[l_,{m_,0}] = 0;
 
sigma[l_,{m_,n_}] := I/2 tr[l,{m,n}] - I/2 tr[l,{n,m}];
 
 
(*-------------- TRACES WITH AN ODD NUMBER OF GAMMA-MATRICES -------------*)
 
tr[l_,{a___}] := 0 /; ( OddQ[Length[{a}]-Count[{a},G5]] && 
							MemberQ[CalcTable,l] );
 
 
(*------------------- REDUCTION TO TERMS WITH ONLY ONE G5 ----------------*) 
 
tr[l_,{a___, G5, G5, b___}] := tr[l,{a, b}];
 
(* Use cyclicity of "tr" *)
 
tr[l_,{a___,G5,b__}] := tr[l,{b,a,G5}] /; ( !MemberQ[{a,b},G5] && 
							MemberQ[CalcTable,l] );
 
 
(*----------------------- ANTICOMMUTING G5 AS DEFAULT --------------------*)
 
(* 
 * Anticommuting G5 can be toggled later with AntiCommute[on/off]. 
 * The rule MUST be stated here to place it at the proper position in the 
 * rule tables build up by Mathematica for "tr". One can NOT simply remove 
 * or reformulate it depending on "G5flag" since this would place it at
 * the wrong position (most likely at the end) in the rule tables.
 *)
 
tr[l_,{a___, G5, b_, c___}] := - tr[l,{a, b, G5, c}] /; G5flag;
 
 
(*-------------------- TRACES OF UP TO 4 GAMMA-MATRICES ------------------*)
 
(* 
 * Give some explict formulae for short traces to improve performance 
 * since long traces will be reduced to many ever shorter traces when 
 * anti/commuting gamma matrices.
 *)
 
tr[l_,{}] := 4  /;  MemberQ[CalcTable,l];
 
tr[l_,{G5}] := 0  /;  MemberQ[CalcTable,l];
 
tr[l_,{a_, b_, G5}] := 0  /;  MemberQ[CalcTable,l];
 
tr[l_,{a_, b_}] := 4 S[a,b]  /;  MemberQ[CalcTable,l];
 
tr[l_,{a_, b_, c_, e_, G5}] := 4I eps[{a, b, c, e}] /; MemberQ[CalcTable,l];
 
tr[l_,{a_, b_, c_, e_}] := 4 S[a,b] S[c,e] - 4 S[a,c] S[b,e] + 
			  4 S[a,e] S[b,c]  /;  MemberQ[CalcTable,l];
 
 
(*---------------- CONTRACTION OF GAMMA-MATRICES ON ONE LINE -------------*)
(*         See references listed in program head for more details.        *)
 
tr[l_,{a___,H@{mu_},b___,  {mu_},c___}] := tr[l,{a, H@{mu}, b, H@{mu}, c}];
tr[l_,{a___,  {mu_},b___,H@{mu_},c___}] := tr[l,{a, H@{mu}, b, H@{mu}, c}];
 
tr[l_,{a___, mu_, mu_, b___}] := Switch[ mu,
					          _List, d tr[l,{a, b}],
					       H[_List], (d-4) tr[l,{a, b}],
					              _, S[mu,mu] tr[l,{a, b}]
                                       ];
 
tr[l_,{a___, mu_, b_, mu_, c___}] :=
   Switch[ mu,
           _List   , (2-d) tr[l,{a,b,c}],
           H[_List], (4-d) tr[l,{a,b,c}] + 2 tr[l,{a,H@b,c}],
                  _, 2 S[mu,b] tr[l,{a,mu,c}] - S[mu,mu] tr[l,{a,b,c}]
         ];
 
tr[l_,{a___, mu_, b_, c_, mu_, e___}] :=
   Switch[ mu,
       _List, 4 S[b,c] tr[l,{a, e}] + (d-4) tr[l,{a, b, c, e}],
    H[_List], (d-4) tr[l,{a,b,c,e}]-2 tr[l,{a,H@b,c,e}]+2 tr[l,{a,H@c,b,e}],
           _, S[mu,mu] tr[l,{a, b, c, e}] - 2 S[mu,b] tr[l,{a, mu, c, e}] +
              2 S[mu,c] tr[l,{a, mu, b, e}]
         ];
 
tr[l_,{a___, mu_, b_, c_, e_, mu_, f___}] :=
   Switch[ mu,
          _List, (4-d) tr[l,{a,b,c,e,f}] - 2 tr[l,{a, e, c, b, f}],
          H[_List], (4-d) tr[l,{a,b,c,e,f}] + 2 tr[l,{a,H@b,c,e,f}] -
                     2 tr[l,{a,H@c,b,e,f}] + 2 tr[l,{a,H@e,b,c,f}],
          _, 2 S[mu,b] tr[l,{a,mu,c,e,f}] - 2 S[mu,c] tr[l,{a,mu,b,e,f}] +
		   2 S[mu,e] tr[l,{a,mu,b,c,f}] - S[mu,mu] tr[l,{a,b,c,e,f}]
         ];
 
tr[l_,{a___, mu_, b__, mu_, c___}] := 
  Switch[ mu,
        _List, Expand[
		     (-1)^Length[{b}] ( (d-4) tr[l,{a,b,c}] +
		     2 tr[l,Flatten[ {{a},Join[Reverse[Drop[{b},3-Length[{b}]]],
				  Drop[{b},3]], {c} }, 1] ] +
		     Block[ {i}, 
			   2 Sum[ (-1)^i tr[l, Flatten[ { {a}, Prepend[Drop[{b},
				 {i}], {b}[[i]]], {c} },1 ] ],{i,4,Length[{b}]}
				] 
			  ] ) ],
       H[_List], Expand[
		     (-1)^Length[{b}] ( (d-4) tr[l,{a,b,c}] +
		     Block[ {i}, 
			   2 Sum[ (-1)^i tr[l, Flatten[ { {a}, Prepend[Drop[{b},
				 {i}],H@{b}[[i]]],{c} },1 ] ],{i,1,Length[{b}]}
				] 
			  ] ) ],
	       _, Expand[
		      (-1)^Length[{b}] ( S[mu,mu] tr[l,{a,b,c}] +
		      Block[ {i},
			    2 Sum[ (-1)^i S[mu, {b}[[i]] ] 
				  tr[l,Flatten[{{a},mu,Drop[{b},{i}],{c}},1]],
				      {i,1,Length[{b}]}
				 ]
			   ] ) ]
	  ];
 
 
(*------------------ TRACES OF MORE THAN 4 GAMMA-MATRICES ----------------*)
(*         See references listed in program head for more details.        *)
 
tr[l_,{a__, G5}] := Expand[
   Block[ {i}, Sum[(-1)^i S[{a}[[1]],{a}[[i]]] *
      tr[l,Append[Rest[Drop[{a},{i}]], G5]], {i,2,Length[{a}]} ] ] -
   I/6 eps[{{a}[[1]],{i1},{i2},{i3}}] tr[l,Join[Rest[{a}],{{i1},{i2},{i3}} 
   ]]]  /;  MemberQ[CalcTable,l];
 
tr[l_,{a__}] := Expand[
		      Block[ {i}, Sum[(-1)^i S[{a}[[1]],{a}[[i]]] *
			      tr[l,Rest[Drop[{a},{i}]]],{i,2,Length[{a}]} ] ] 
		     ]  /;  MemberQ[CalcTable,l];
 
 
(*----------- PROJECTION PROPERTIES OF HAT AND TILDE OPERATORS -----------*)
 
H[0] = 0;
H[ H[p__] ] := H[p];
 
tr /: a1___ tr[ l1_,{b1___, H@{mu_}, b2___}] a2___ *
            tr[ l2_,{c1___,   {mu_}, c2___}] a3___ :=
      a1 tr[ l1, {b1,H@{mu},b2}] a2 tr[ l2, {c1,H@{mu},c2}] a3;
 
tr /: a1___ tr[ l1_,{b1___,   {mu_}, b2___}] a2___ *
            tr[ l2_,{c1___, H@{mu_}, c2___}] a3___ :=
      a1 tr[ l1, {b1,H@{mu},b2}] a2 tr[ l2, {c1,H@{mu},c2}] a3;
 
tr /: a1___ ** tr[ l1_,{b1___, H@{mu_}, b2___}] ** a2___ **
               tr[ l2_,{c1___,   {mu_}, c2___}] ** a3___ :=
      a1 ** tr[ l1, {b1,H@{mu},b2}] ** a2 ** tr[ l2, {c1,H@{mu},c2}] ** a3;
 
tr /: a1___ ** tr[ l1_,{b1___,   {mu_}, b2___}] ** a2___ **
               tr[ l2_,{c1___, H@{mu_}, c2___}] ** a3___ :=
      a1 ** tr[ l1, {b1,H@{mu},b2}] ** a2 ** tr[ l2, {c1,H@{mu},c2}] ** a3;
 
T[0] = 0;
T[ T[p__] ] := T[p];
 
 
(*-------------------  LINEARITY OF THE SCALAR PRODUCT -------------------*)
 
Attributes[S] = {Orderless};
Attributes[H] = {Orderless};
Attributes[T] = {Orderless};
 
S[0,p_] = 0;
S[  a_,H@b_] := H[a,b];
S[H@a_,H@b_] := H[a,b];
S[  a_,T@b_] := S[a,b] - H[a,b];
S[T@a_,T@b_] := S[a,b] - H[a,b];
 
H[0,p_]    = 0;
H[T@_,___] = 0;
H[  a_,H@b_] := H[a,b];
H[H@a_,H@b_] := H[a,b];
 
T[H@_,___] = 0;
T[ a_, b_] := S[ a, b] - H[ a, b];
 
S[c_?NumberQ p_, q_]    := c S[p,q];
S[p_, {c_?NumberQ q_} ] := c S[p,{q}];
 
S[p_ + q_, r_] := S[p,r] + S[q,r];
 
 
(*----------------- CONTRACTION RULES FOR THE SCALAR PRODUCT -------------*)
 
(* 
 * Reduce "p.p" type expressions to a real quadrad of p. 
 * S[p_Symbol,p_] := p^2
 *)
 
S[{mu_},{mu_}] := d;
H[{mu_},{mu_}] := d-4;
 
S /: S[{mu_},p_]^2 := S[p,p];
H /: H[{mu_},p_]^2 := H[p,p];
 
S /: S[{mu_},p_] tr[l_, { a___,{mu_},b___ }] := tr[l, { a,  p,b }];
H /: H[{mu_},p_] tr[l_, { a___,{mu_},b___ }] := tr[l, { a,H@p,b }];
 
S /: S[{mu_},p_] tr[l_, { a___,H@{mu_},b___ }] := tr[l, { a,H@p,b }]; 
H /: H[{mu_},p_] tr[l_, { a___,H@{mu_},b___ }] := tr[l, { a,H@p,b }];
 
S /: S[{mu_},p_] e___ ** tr[l_, { a___,{mu_},b___ }] ** f___ :=
     e ** tr[l, { a,  p,b }] ** f;
H /: H[{mu_},p_] e___ ** tr[l_, { a___,{mu_},b___ }] ** f___ :=
     e ** tr[l, { a,H@p,b }] ** f;
 
S /: S[{mu_},p_] e___ ** tr[l_, { a___,H@{mu_},b___ }] ** f___ :=
     e ** tr[l, { a,H@p,b }] ** f;
H /: H[{mu_},p_] e___ ** tr[l_, { a___,H@{mu_},b___ }] ** f___ :=
     e ** tr[l, { a,H@p,b }] ** f;
 
S /: S[{mu_},p_] f_[a___,{mu_},b___] := f[a,  p,b];
H /: H[{mu_},p_] f_[a___,{mu_},b___] := f[a,H@p,b];
 
S /: S[{mu_},p_] f_[a___,H@{mu_},b___] := f[a,H@p,b];
H /: H[{mu_},p_] f_[a___,H@{mu_},b___] := f[a,H@p,b];
 
S /: S[{mu_},p_] f_[{a___,{mu_},b___}] := f[{a,  p,b}];
H /: H[{mu_},p_] f_[{a___,{mu_},b___}] := f[{a,H@p,b}];
 
S /: S[{mu_},p_] f_[{a___,H@{mu_},b___}] := f[{a,H@p,b}];
H /: H[{mu_},p_] f_[{a___,H@{mu_},b___}] := f[{a,H@p,b}];
 
 
(*------------------------------ EPSILON-TENSOR --------------------------*)
 
eps[{___, 0, ___}] = 0;
 
eps[{___, H@p_, ___}] = 0;
 
(*
 * Below we have to use named patterns a___ and b___ because TRACER
 * knows already how to contract identical indices and would therefore
 * contract tr[l, {BlankNullSequence[], H@{mu_}, BlankNullSequence[]}]
 * in the following rule. Weird but consistent. As a workaround one
 * could move the rule further to the beginning of the code and tag on
 * "tr" but for performance reasons we prefere to tag on "eps".
 *)
eps /: eps[{___, {mu_}, ___}] tr[l_, {a___, H@{mu_}, b___}] = 0;
 
eps[list4_] := 0 /; Signature[list4]==0;
 
(* "OrderedQ" must be there to prevent an infinite loop. *)
 
eps[list4_] := Signature[list4] eps[Sort[list4]] /; OrderedQ[list4]==False;
 
eps[{a___, c_?NumberQ p_, b___}]   := c eps[{a, p, b}];
eps[{a___, {c_?NumberQ p_}, b___}] := c eps[{a , {p}, b}];
 
eps[{a___, p_+q_, b___}] := eps[{a, p, b}] + eps[{a, q, b}];
 
 
(* Once again, give some explict formulae to improve performance. *)
 
eps /: eps[{{la_},{mu_},{nu_},p1_}]^2 := 6 ( H[p1,p1] - S[p1,p1] );
 
eps /: eps[{{la_},{mu_},{nu_},p1_}] eps[{{la_},{mu_},{nu_},q1_}] :=
	6 ( H[p1,q1] - S[p1,q1] );
 
eps /: eps[{{mu_},{nu_},p1_,p2_}]^2 :=
	2 Expand[ - Det[{{S[p1,p1]-H[p1,p1],S[p1,p2]-H[p1,p2]},
			 {S[p2,p1]-H[p2,p1],S[p2,p2]-H[p2,p2]}}]];
 
eps /: eps[{{mu_},{nu_},p1_,p2_}] eps[{{mu_},{nu_},q1_,q2_}] :=
	2 Expand[ - Det[{{S[p1,q1]-H[p1,q1],S[p1,q2]-H[p1,q2]},
			 {S[p2,q1]-H[p2,q1],S[p2,q2]-H[p2,q2]}}]];
 
(* General formulae *)
 
eps /: eps[{p1_,p2_,p3_,p4_}]^2 := Expand[ - Det[{
 {S[p1,p1]-H[p1,p1],S[p1,p2]-H[p1,p2],S[p1,p3]-H[p1,p3],S[p1,p4]-H[p1,p4]},
 {S[p2,p1]-H[p2,p1],S[p2,p2]-H[p2,p2],S[p2,p3]-H[p2,p3],S[p2,p4]-H[p2,p4]},
 {S[p3,p1]-H[p3,p1],S[p3,p2]-H[p3,p2],S[p3,p3]-H[p3,p3],S[p3,p4]-H[p3,p4]},
 {S[p4,p1]-H[p4,p1],S[p4,p2]-H[p4,p2],S[p4,p3]-H[p4,p3],S[p4,p4]-H[p4,p4]}}]
 ];
 
eps /: eps[{p1_,p2_,p3_,p4_}] eps[{q1_,q2_,q3_,q4_}] := Expand[ - Det[{
 {S[p1,q1]-H[p1,q1],S[p1,q2]-H[p1,q2],S[p1,q3]-H[p1,q3],S[p1,q4]-H[p1,q4]},
 {S[p2,q1]-H[p2,q1],S[p2,q2]-H[p2,q2],S[p2,q3]-H[p2,q3],S[p2,q4]-H[p2,q4]},
 {S[p3,q1]-H[p3,q1],S[p3,q2]-H[p3,q2],S[p3,q3]-H[p3,q3],S[p3,q4]-H[p3,q4]},
 {S[p4,q1]-H[p4,q1],S[p4,q2]-H[p4,q2],S[p4,q3]-H[p4,q3],S[p4,q4]-H[p4,q4]}}]
 ];
 
(* decompose Eps[p1,p2,p3,p4] g_q1 g_q2 g_q3 for anticommuting G5 *)
 
eps /: eps[{p1_,p2_,p3_,p4_}] tr[l,{q1_,q2_,q3_}] :=
Block[ {m},
       (* unique auxiliary index to prevent unindended contractions *)
       m = Unique["idx"];  
       eps[{p1,p2,p3,p4}] ( S[q1,q2] tr[l,{q3}] + S[q2,q3] tr[l,{q1}] - 
       S[q1,q3] tr[l,{q2}] + I eps[{q1,q2,q3,{m}}] tr[l,{G5,{m}}] ) /; 
         G5flag && Length[Intersection[{p1,p2,p3,p4},{q1,q2,q3}]] >= 1
     ];
 
(* perform contraction for index pairs in eps_{...,{mu},...} g_mu *) 
 
(* The rhs of the rule below is a little bit complicated since we do not
   know where exactly in indices the generic index {mu_} appears. The 
   reason is that Eps[] automatically rearranges its arguments in
   canonical order e.g. Eps[a, b, {m}, c] G[l, {m}] would be reordered
   while Eps[a, b, {m}, {n}] G[l, {m}] would not. Thus the number of
   explicit rules taking into account all possible argument types /
   orderings would be tremendous. *)
 
ContractEpsGammaRule =
   eps[indices_List] tr[l_, {a___, {mu_}, b___}] :>
      Block[ {index, pos},
             pos   = Position[indices, {mu}][[1,1]];
             index = Drop[indices, {pos,pos}]; 
             (-1)^pos * (
             I tr[l, {a, T@index[[1]], T@index[[2]], T@index[[3]], G5, b}] -
             I T[index[[1]],index[[2]]] tr[l, {a, T@index[[3]], G5, b}] +
             I T[index[[1]],index[[3]]] tr[l, {a, T@index[[2]], G5, b}] -
             I T[index[[2]],index[[3]]] tr[l, {a, T@index[[1]], G5, b}]
             )
      ] /; ( Length[indices]==4 && MemberQ[indices, {mu}] );
 
ContractEpsGamma[expr_] := expr //. ContractEpsGammaRule // Expand;
 
 
(*-------------------------- ONSHELL DECLARATIONS ------------------------*)
 
lookup = {};             (* default lookup table for OnShell declarations *)
 
OnShell[ flag_:query, list___ ] := 
   Block[ { ni, i, l, l3 },
      l = { list };
      Switch[ flag,
	  Tracer`on,
	     ( Unprotect[ S ];
             For[i=1, i <= Length[l], i++,
		   ni=Length[ l[[i]] ];
		   Switch[ ni,
                   2, l3 = {l[[i,1]], l[[i,1]], l[[i,2]]}, 
		       3, l3 = l[[i]],
		       _, (Print[ "Illegal OnShell declaration: Probably",
				 " too many/less elements in list ", l[[i]], "."];
                       Continue[]) ];
		       S[ l3[[1]],l3[[2]] ] = l3[[3]]; 
			 lookup = Union[lookup, {l3}]
		 ];
	       Protect[ S ]
           ),
	  Tracer`off,
           ( Unprotect[ S ];
             For[i=1, i <= Length[l], i++,
               ni=Length[ l[[i]] ];
               Switch[ ni,
                   2, l3 = {l[[i,1]], l[[i,1]], l[[i,2]]},
                   3, l3 = l[[i]],
                   _, (Print[ "Illegal OnShell declaration: Probably",
                         " too many/less elements in list ", l[[i]], "."];
                      Continue[]) ];
                   If[ MemberQ[lookup, l3],
                       ( S[ l3[[1]],l3[[2]] ] =. ;
                         lookup = Complement[lookup, {l3}] ),
                       Print["No such OnShell declaration: ",l[[i]]]
                     ]
             ];
             Protect[ S ]
           ),
	  query, If[ Length[lookup] != 0, 
		     ( Print[ "Active OnShell declarations:" ];
		       Do[ Print[ StoString[lookup[[i,1]],lookup[[i,2]]],
				  " = ", S[ lookup[[i,1]],lookup[[i,2]] ] ],
			   { i, Length[lookup] }
			 ] ), 
		       Print[ "No OnShell declarations are active." ]
		   ],
	  _, Print["Illegal flag in OnShell."] ];
	 ];
 
(*--------------- SORT GAMMA MATRICES ON LINES BY REFERENCE --------------*)
 
(* 
 * Sort "list" on line "l" in expression "expr" with reference to 
 * "reflist".
 * "recflag" controlls the usage of the "G5" rules during the recursive
 * calls to "SortLine" in "SortRules". During the reordering the
 * rules commuting "G5" to the right are disabled while at the end the
 * relevant rules are enabled again.
 *)
 
SortLine[ expr_, l_, reflist_List:{}, recflag_:query] := Block[
   { i, k, list, perm={}, out1=expr, out2, pos, rlist=reflist, slot, x },
   G5flag := False;
   pos = Position[ expr, tr[ l, {___}] ];
   (* if requested line not found return expr unsorted *)
   If[ pos == {}, Return[expr] ];
   Do[
	list = expr[[ pos[[i]] /. List[a___] -> a, 2 ]];
	If[ reflist == {},
	    If[ MemberQ[ list, G5],
		rlist = Append[ Sort[ Drop[ list, {Where[list,G5]}] ], G5],
		rlist = Sort[list] ]
	  ];
	If[ list === rlist, ,
	    If[ Sort[list] === Sort[rlist],
		perm={};
		Do[ 
		    slot = Where[ list, rlist[[k]] ];
		    perm = Append[ perm, slot], {k, Length[list]}
		  ];
		out2 = tr[ l, list];
		Do[
		    x = list[[ perm[[k]] ]];
		    SetSortRules[ l, rlist, x, k ];
		    out2 = out2 //. SortRules
		    ,{ k, Length[perm] }
		  ];
		out1 = out1 /. tr[ l, list] -> out2 
	      ]
	  ], {i, Length[pos]}
     ];
   Return[ If[ recflag == query, G5flag := True] ; Expand[ out1 ] ] ];
 
(*
 * Utilities for "SortLine"
 *)
 
(* Where is "x" in "list" ? *)
 
Where[ list_List, x_] := First[ Sort[ Position[ list, x] ] ][[1]];
 
(* 
 * Set up the transformation rule for current list element "x" to be
 * moved to position "i" on line "l".
 *)
 
SetSortRules[ lidx_, rlist_, x_, k_] :=
(
 SortRules =
 Switch[  x,
	  G5, tr[ lidx, {a___, b_, G5, c___} ] :>
	      Switch[ Head[b],
		      H, tr[lidx, {a, G5, b, c}],
		      _, If[ AntiComm, -tr[lidx, {a, G5, b, c}], 
			 -tr[lidx, {a,G5,b,c}] + 2 tr[lidx, {a,G5,H@b,c}] ]
		    ] /; Where[{a,b,G5,c},G5] > k,
	   _,
	 {
	   tr[ lidx, {a___, G5, x, c___} ] :> 
	       Switch[ Head[x],
		       H,  tr[lidx, {a, x, G5, c}],
		       _,  If[ AntiComm, -tr[lidx, {a, x, G5, c}],
			   -tr[lidx, {a,x,G5,c}] + 2 tr[lidx, {a,H@x,G5,c}] ]
		     ] /; Where[{a,G5,x,c},x] > k
	   ,
	   tr[ lidx, {a___, b_, x, c___} ] :> -tr[ lidx,{a, x, b, c}] +
	       2 S[b,x] SortLine[ tr[ lidx,{a,c} ], lidx, Drop[ Drop[ rlist,
	       {Where[rlist,b]} ], {Where[rlist,x]} ], deep ]
	       /; ( Where[{a,b,x,c},x] > k ) && Not[b===G5]
	 }
	];
  SortRules = Dispatch[ SortRules ]
);
 
 
(*------------ IMPROVE READABILITY BY COLLECTING 1-+G5 TERMS ------------*)
 
(*
 * "ToUG5" tries to transform terms of the form "g_mu +- g_mu g_5" 
 * to "g_mu ( 1 +- g_5 )".
 * Note that one can NOT transform this in a straightforward way to 
 * "tr[l,{{mu},U-+G5}]" since this would match an earlier pattern and
 * thus lead to an infinite loop. 
 * Therefore the appropriate rules for expansion of terms matching the
 * pattern "tr[l,{{mu},U-+G5}]" are controlled by the flag "ApartUG5".
 * This situation is analogous to "G5flag" in the function "SortLine".
 *)
 
(*
 * Rules for "1-+g_5" collection
 * Extracting numbers, symbols and combinations of both and combining
 * the rest needs a careful choice of the relevant rules. Especially 
 * the "Minus" sign in expressions like "g_mu - g_mu g_5" is a bit
 * tricky since it is stored internally as "Times[-1,g_mu]". Use 
 * "FullForm" to understand the Why's and How's of the rules.
 * The order of rules in "ToUG5" is absolutely crucial for proper 
 * operation of the "1+-g_5" collection.
 * E.g. with reversed rule order the expanded form of "G[l1,U-G5] *
 * G[l2,U-G5]" is only partially collected.
 *)
 
(*
 * Note the `dirty trick' concerning the "Return[...]" statement!
 * Setting "ApartUG5 := True" just before the "Return" and doing a
 * simple "Return[out]" would invoke an evaluation of "out" with the 
 * U-+G5 expansion already in use again. 
 * There is nothing about that in the manual but it seems to work. ;-)
 *)
 
GetAllLines[expr_] :=
  Block[ {lines={}, pos, l, i},
         pos = Position[expr, tr[___]];
         Do[
             l = expr[[ pos[[i]] /. List[a___] -> a, 1 ]];
             AppendTo[lines, l];
             , {i,1,Length[pos]}
           ];
         lines = Union[lines];
         lines
       ];
 
(*
 * We have to use "rule = ... :> ... //. {lidx -> l}" because otherwise
 * we won't get line indices "l" properly substituted with "lines[[i]]"
 * on the rhs of ":>".
 * Note: We have to write`m*1' because we use `m__' as pattern on 
 *       the lhs.
 *)
SetToUG5Rules[lines_] :=
  Block[ {lidx, l, i, rule},
         ToUG5Rules = {};
         Do[
             l = lines[[i]];
             rule=m__ tr[lidx,{a___,b___}]+n__ tr[lidx,{a___,G5,b___}]:>
                  (m*1+n*1) 1/2 tr[lidx,{a,U+G5,b}] + 
                  (m*1-n*1) 1/2 tr[lidx,{a,U-G5,b}];
             rule = rule //. {lidx -> l}; 
             AppendTo[ToUG5Rules, rule];
 
             rule=m_. tr[lidx,{a___,b___}]+n_. tr[lidx,{a___,G5,b___}]:>
                  (m+n) 1/2 tr[lidx,{a,U+G5,b}] + 
                  (m-n) 1/2 tr[lidx,{a,U-G5,b}];
             rule = rule //. {lidx -> l}; 
             AppendTo[ToUG5Rules, rule];
 
             rule= m__ (e___ ** tr[lidx,{a___,b___}   ] ** f___) +
                   n__ (e___ ** tr[lidx,{a___,G5,b___}] ** f___) :>
                  (m*1+n*1) 1/2 (e ** tr[lidx,{a,U+G5,b}] ** f) + 
                  (m*1-n*1) 1/2 (e ** tr[lidx,{a,U-G5,b}] ** f);
             rule = rule //. {lidx -> l}; 
             AppendTo[ToUG5Rules, rule];
 
             rule=m_. (e___ ** tr[lidx,{a___,b___}   ] ** f___) +
                  n_. (e___ ** tr[lidx,{a___,G5,b___}] ** f___) :>
                  (m+n) 1/2 (e ** tr[lidx,{a,U+G5,b}] ** f) + 
                  (m-n) 1/2 (e ** tr[lidx,{a,U-G5,b}] ** f);
             rule = rule //. {lidx -> l}; 
             AppendTo[ToUG5Rules, rule]
(*
 * Shall we include that ?
 *
 *            rule = tr[lidx,{a___,G5,b___}] :> 
 *                      1/2 tr[lidx,{a,U+G5,b}] + 1/2 tr[lidx,{a,U-G5,b}];
 *            rule = rule //. {lidx -> l}; 
 *            AppendTo[ToUG5Rules, rule]
 *)
             , {i,1,Length[lines]}
           ];
         ToUG5Rules = Dispatch[ToUG5Rules]
       ];
 
 
ToUG5[ expr_, lines_:{} ] := 
 Block[ {out, linelist=lines, pos, poslist, i},
	   ApartUG5 := False;
	   G5flag   := False;
        If[linelist == {}, linelist = GetAllLines[expr]];
        SetToUG5Rules[linelist];
	   out = ExpandAll[ expr ] //. ToUG5Rules;
(* 
 * We have to do a MapAt[Release,...] here because the above ExpandAll
 * gets inside possible Hold[]'s in "out". There it would cause commands
 * like TeXForm[ToUG5[expr]] to produce garbage.
 *)
        poslist = Position[out, Hold[_]];
        Do[
             pos = poslist[[i]];
             (* argument of Hold[_] is one level deeper *)
             AppendTo[pos, 1];  
             out = MapAt[Release, out, pos];
             ,{i,Length[poslist]}
          ];
	   Return[ApartUG5 := True; G5flag := True; out]
	 ];
 
 
(*-------------- TeX FORMAT FOR `*' PRODUCT OF SEVERAL LINES  ------------*)
 
(*
 * Note that the ordering of rules in "OtimesRules" is crucial to give a 
 * one-to-one copy of the "G[...]*G[...]*..." order in "Otimes" and
 * therefore in the final TeX output.
 * Since Mathematica orders products of expressions of type "tr[l,{...}]" 
 * with respect to the first argument one can enforce a certain order of 
 * lines in "G[...]*G[...]*..." by naming line indices appropriately, 
 * e.g. "G[l1,...]*G[l2,...]*...".
 *)
 
OtimesRules = {
		 otimes[a__] b_tr :> otimes[ a,b ]
		 ,
		 a_tr b_tr        :> otimes[ a,b ]
	       };
 
ToOtimes[ expr_ ] := expr //. OtimesRules;
 
 
Format[ otimes[a__], TeXForm ] := 
  Block[ { i,strg="" },
	  Do[
	       strg = StringJoin[ strg, ToString@TeXForm[{a}[[i]]], 
				  " \\otimes " ];
	     , {i,Length[{a}]-1}
	    ];
	  strg = StringJoin[ strg, ToString@TeXForm[{a}[[Length[{a}]]]] ]
	];
 
(*------------------ DECOMPOSE STRINGS OF GAMMAS IN d=4  -----------------*)
 
(*
 * Decomposition of strings of gamma matrices into 1-, g_5-, g_mu-, 
 * g_mu g_5- and 1/2 [ g_mu,g_nu ]-terms in "4" space-time dimensions.
 * The decomposition is done with brute force by projecting the gamma 
 * string on the various basis terms.
 *)
 
ToDiracBasis[ expr_, lines_:{} ] := 
 Block[ {linelist=lines,poslist={},DecomposeRules={},FermionLine,i},
        (* get indices of lines to decompose *)
        If[linelist == {}, linelist = GetAllLines[expr]];
        (* where in expr are the fermion lines *)
        Do[ poslist = Join[ poslist,
                            Position[expr, tr[linelist[[i]], {___}]]
                          ];
            , {i, Length[linelist]}
          ];
        (* build list of rules to decompose each indiviual f. line *)
        Do[ FermionLine = expr[[ poslist[[i]] /. List[a___] -> a ]];
            AppendTo[ DecomposeRules,
                      FermionLine -> ToDiracBasisSingle[FermionLine]
                    ]
            , {i, Length[poslist]}
          ];
        (* replace each fermion line by its' decomposition *)
        expr //. DecomposeRules
      ];
 
(*
 * Note: We have to do the decomposition in a single final step with
 * "expr //. DecomposeRules" because line by line replacement would
 * leave the predetermined positions for later fermion lines invalid.
 *)
 
(*
 * Decomposition to {1, g_5, g_mu, g_mu g_5, sigma_{mu nu}} basis for a
 * single fermion line.
 *)
 
ToDiracBasisSingle[ expr_tr ] :=
   Block[ { list, l1, l2, m1, m2, out, AuxSigma },
          (* ensure decomposition to basis for d=4 only *)
          If[ d =!= 4, Return["Error: Use \"VectorDimension[4]\" first."] ];
          (* extract line and gamma string indices *)
          l1   = expr[[1]];
          list = expr[[2]];
          (* unique auxiliary indices to prevent unintended contractions *)
          l2   = Unique["ln"];  
          m1   = Unique["idx"];
          m2   = Unique["idx"];
          (* projections on complete basis for d=4 *)
          CalcTable = Union[CalcTable, {l2}];
          scalar  = 1/4 tr[ l2,list ];
          pscalar = 1/4 tr[ l2,Append[list,G5] ];
          vector  = 1/4 tr[ l2,Append[list,{m1}] ];
          pvector = 1/4 tr[ l2,Append[Append[list,G5],{m1}] ];
          tensor  = -I/8 (1/2 tr[ l2,Append[Append[list,{m2}],{m1}] ] -
                          1/2 tr[ l2,Append[Append[list,{m1}],{m2}] ] );
          (* free local line index l2 from being traced *)
          CalcTable = Complement[CalcTable, {l2}];
          out = ExpandAll[ scalar  tr[l1,{U}] +
                           pscalar tr[l1,{G5}] + 
                           vector  tr[l1,{{m1}}] +
                           pvector tr[l1,{{m1},G5}] + 
                           tensor  AuxSigma[l1,{{m1},{m2}}]
                         ];
          (* contract and order indices of the auxiliary sigma matrix *)
          out = out //. {
             S[{mu_},p_] AuxSigma[l_, { a___,{mu_},b___ }] :>
                AuxSigma[l, { a,  p,b }],
             S[{mu_},p_] AuxSigma[l_, { a___,H@{mu_},b___ }] :>
                AuxSigma[l, { a,H@p,b }],
             AuxSigma[l_,list_List] :> Signature[list] *
                AuxSigma[l,Sort[list]] /; OrderedQ[list]==False
          }; 
          out = out //. AuxSigma[l_,list_List] :> sigma[l,Hold[list]];
          (*
           * Memo: Usage of an intermediate AuxSigma[] allows argument
           * {{m1},{m2} to be evaluated, contractions and ordering to 
           * be done; the final sigma[l1,Hold[]] prevents the automatic
           * sigma decomposition.
           *)
          (* garbage collection: remove obsolete auxiliary indices *)
          Remove[ Release[l2] ];
          If[ FreeQ[out,Release[m1]], Remove[ Release[m1] ] ];
          If[ FreeQ[out,Release[m2]], Remove[ Release[m2] ] ];
          (* return the resulting decomposition *)
          out
        ];
 
(* 
 * It is not possible to do garbage collection with the statement
 * "Remove[ Release[l2], Release[m1], Release[m2] ];".
 * This would work if all indices "m1","m2" in the tr's of "out" would 
 * have been already contracted with their counterparts in "vector", etc. 
 * and therefore "m1","m2" would be obsolete and ready to be removed.
 * However, in terms like "Eps[{m},{n},{l},{idx1}] G[l,I.{idx1},G5]" 
 * "idx1" can not be removed. Therefore the index number unfortunately
 * will increase slowly with continued invocations of "ToDiracBasis".
 *)
  
 
(*----------- UTILITIES FOR HANDLING OF HAT- AND TILDE-PARTS  ------------*)
 
ToHatTildeRules = Dispatch[
 {
   tr[lidx_, {a___, p_Symbol, b___}] :> tr[lidx, {a, Hold[T@p], b}] + 
     tr[lidx, {a, H@p, b}] /; p =!= G5
   ,
   tr[lidx_, {a___, {m_}, b___}] :>
     tr[lidx, {a, Hold[T@{m}], b}] + tr[lidx, {a, H@{m}, b}]
   ,
   (* Test for Head to avoid an infinite loop *)
   S[p_,q_] :> S[ Hold[T@p],Hold[T@q] ] + H[p,q] /;
                  (Head[p] =!= Hold && Head[q] =!= Hold)
 }
];
 
ToHatTilde[expr_] := expr //. ToHatTildeRules // Expand;
 
RemoveHatMomenta[expr_, p__] := 
   Block[ {plist={p}, q, rule={}, i},
          Do [
               q = plist[[i]];
               AppendTo[rule, H[q]   -> 0];
               AppendTo[rule, H[q,_] -> 0];
               , {i,Length[plist]}
             ];
          rule = Dispatch[rule];
          expr //.  rule
        ];
 
 
(*-------------- ADDITIONAL RULES FOR NONCOMMUTATIVEMULTIPLY -------------*)
 
(*
 * These rules have to be placed at the end of Tracer, because otherwise
 * they would match left-hand sides of previous rules and hence alter them.
 *)
 
Unprotect[NonCommutativeMultiply];
 
a___ ** 0 ** e___ = 0;
 
a___ ** Times[b_,c__] ** e___ := b a ** Times[c] ** e /; Not[Head[b]===tr];
 
a___ ** Plus[ b_,c__] ** e___ := a ** b ** e + a ** Plus[c] ** e;
 
a___ ** (s_ tr[l_, {b___}]) ** c___ :=
   s a ** tr[l, {b}] ** c /; Not[ Head[s] === tr ];
 
NonCommutativeMultiply[a__] := Times[a] /; NCMflag &&
   If[ FreeQ[{a}, tr], False,
       Block[ { i, list={}, pos},
       pos = Position[ {a}, tr[___] ];
       Do[ list = Append[ list, {a}[[ pos[[i]] /. List[ex___]->ex, 1 ]] ]
          ,{i, Length[pos]}
         ];
       Union[list] === Sort[list] ]
     ];
 
Protect[NonCommutativeMultiply];
 
tr /: tr[l_,{a___}] ** b__ := b tr[l,{a}] /; FreeQ[{b}, tr];
 
tr /: b__ ** tr[l_,{a___}] := b tr[l,{a}] /; FreeQ[{b}, tr];
 
tr /: tr[l_,{a___}] ** tr[l_,{b___}] := tr[l,{a,b}];
 
tr /: a___ ** tr[l_,{b___}] ** f__ ** tr[l_,{c___}] ** e___ :=
      a ** tr[l,{b,c}] ** f ** e /; FreeQ[{f}, tr[l,{___}] ];
 
 
(*------------- PROTECT SPECICAL SYMBOLS FROM ACCIDENTAL USE -------------*)
 
Protect[ AntiCommute, ContractEpsGamma, Eps, GTrace, G5, H,
         ListCommands, NoSpur, OnShell, OutputFormat, RemoveHatMomenta,
         RemoveNCM, S, Sigma, SortLine, Spur, T, ToDiracBasis,
         ToHatTilde, ToOtimes, ToUG5, U, VectorDimension, Version ];
 
 
(*---------------------------- CLOSE CONTEXTS ----------------------------*)
 
End[ (* Tracer`Private` *) ];
 
EndPackage[ (* Tracer` *) ];
 
 
(*--------------- DISPLAY DEFAULT SETTINGS ON STARTUP --------------------*)
 
RemoveNCM[ on ];
 
OutputFormat[ texlike ];
 
AntiCommute[ off ];
 
 
(*======================= END OF FILE Tracer-1.1.m =======================*)
