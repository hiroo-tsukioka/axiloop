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


BeginPackage["Axiloop`Exception`"]

Raise::usage = "";

Try::usage = "";


Begin["`Private`"]

Raise[exception_] := Module[{},
	Throw[exception, $DefaultExceptionTag];
];

SetAttributes[Try, HoldAll];
Try[expr_, handler_: Null] := Module[{result},
	result = Catch[
		expr
		,
		$DefaultExceptionTag
		,
		Function[{$value, $tag},
			If[handler === Null, $value, handler]
		]
	];
	result
];
 

End[]

EndPackage[]