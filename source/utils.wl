BeginPackage["caUtils`"]

(* Utils for piecewise functions *)
pwGetPairs[f_Piecewise] := Module[{vals, cnds},
  vals = Append[f[[1, ;; , 1]], f[[2]]];
  cnds = f[[1, ;; , 2]];
  cnds = Append[cnds, Not@Fold[Or, cnds]];
  {vals, cnds}]
pwAddConditionIgnoreLast[f_Piecewise, cnd_] := Module[{},
  f[[1, ;; , 2]] = # && cnd & /@ f[[1, ;; , 2]]; f
  ]
pwAddCondition[f_Piecewise, cnd_] := Module[{vals, cnds},
  {vals, cnds} = pwGetPairs[f];
  cnds = (# && cnd &) /@ cnds;
  Piecewise[{vals, cnds}\[Transpose], Undefined[]]
  ]

EndPackage[]