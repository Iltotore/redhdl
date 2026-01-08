component Eq1Bit
input
  inA: Boolean,
  inB: Boolean
output
  eq: Boolean
begin
  eq = (inA and inB) or ((not inA) and (not inB))
end