component FullAdder
input
  inA: Boolean,
  inB: Boolean,
  inCin: Boolean
output
  sum: Boolean,
  carry: Boolean
begin
  sum = (inA xor inB) xor inCin
  carry = (inA and inB) or (inCin and (inA xor inB))
end