component Trunk
input
  inA: Boolean,
  inB: Boolean
output
  outA: Boolean,
  outB: Boolean,
  outC: Boolean
begin
  outA = inB
  outB = inA
  outC = inA
end