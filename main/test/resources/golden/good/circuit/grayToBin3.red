component GrayToBin3
input
  g2: Boolean,
  g1: Boolean,
  g0: Boolean
output
  b2: Boolean,
  b1: Boolean,
  b0: Boolean
begin
  b2 = g2
  b1 = g2 xor g1
  b0 = g2 xor g1 xor g0
end