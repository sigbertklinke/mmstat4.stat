f[x_] := Exp[-x] / ((1+Exp[-x])^2)
Integrate[f[x], x]
Integrate[f[x], {x, 0, 1}]
N[1/2 - 1/(1+Exp[1])]