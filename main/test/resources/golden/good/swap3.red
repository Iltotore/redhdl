component Swap3
input
  inA: Boolean,
  inB: Boolean,
  inC: Boolean
output
  outA: Boolean,
  outB: Boolean,
  outC: Boolean
begin
  outA = inC
  outB = inA
  outC = inB
end