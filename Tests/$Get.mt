(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2013 Oleksandr Gituliar.                               *)
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

Get["Tests/core.mt"];


Test[
	$Get[{}, "key", 1]
	,
	1
	,
	TestID->"GetSet-20130122-O8G8A6"
]

Test[
	$Get[{{"key_1", 0}, {"key_2", 2}}, "key_2"]
	,
	2
	,
	TestID->"GetSet-20130122-N3S3N4"
]

Test[
	$Get[{{"key_1", 1}, {"key_2", 2}}, "key_3", 3]
	,
	3
	,
	TestID->"GetSet-20130122-S4W4I4"
]

Test[
	$Get[{{"key_1", {{"key_1_1", 11}, {"key_1_2", 12}}}, {"key2", 2}}
		,
		{"key_1"}
	]
	,
	{{"key_1_1", 11}, {"key_1_2", 12}}
	,
	TestID->"GetSet-20130122-N7O5P4"
]

Test[
	$Get[{{"key_1", {{"key_1_1", 11}, {"key_1_2", 12}}}, {"key2", 2}}
		,
		{"key_1", "key_1_2"}
	]
	,
	12
	,
	TestID->"GetSet-20130122-O6Y2O7"
]
