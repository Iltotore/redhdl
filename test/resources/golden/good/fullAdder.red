component FullAdder
input
  inA: Boolean,
  inB: Boolean,
  inCin: Boolean
output
  sum: Boolean,
  carry: Boolean
begin
  xor1 = inA xor inB

  sum = xor1 xor inCin
  carry = (inA and inB) or (inCin and xor1)
end