(* Unit Tests for Axiloop Package
 *
 * Author:   Oleksandr Gituliar <gituliar@gmail.com>
 * Created:  05 June 2012
 *
 * Copyright (c) 2012 Oleksandr Gituliar
 *)

Needs["Axiloop`"]

(* EquivalenceFunction for Test. It turns out that Equal doesn't do the job. *)
EqualSimplify[x_, y_] := SameQ[0, Simplify[x-y]];