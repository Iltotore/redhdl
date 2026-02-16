component ManyInputsManyOutputs
input
  V1: Boolean,
  V2: Boolean,
  G1: Boolean,
  G2: Boolean,
  N1: Boolean,
  N2: Boolean,
  c: Boolean,
  d: Boolean,
  g: Boolean,
  h: Boolean,
  j: Boolean
output
  T1: Boolean,
  T2: Boolean,
  F: Boolean,
  L1: Boolean,
  L2: Boolean,
  b: Boolean,
  f: Boolean,
  i: Boolean,
  dummy_out: Boolean
begin
  T1 = d
  T2 = V1
  F = G1
  L1 = h
  L2 = N1
  b = c
  f = g
  i = j

  dummy_out = V2 and G2 and N2
end