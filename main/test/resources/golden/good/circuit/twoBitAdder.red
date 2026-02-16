component TwoBitAdder
input
  inA0: Boolean,
  inA1: Boolean,
  inB0: Boolean,
  inB1: Boolean,
  inCin: Boolean
output
  sum0: Boolean,
  sum1: Boolean,
  carry: Boolean
begin
  sum0 = (inA0 xor inB0) xor inCin

  sum1 = (inA1 xor inB1) xor ((inA0 and inB0) or (inCin and (inA0 xor inB0)))
  
  carry = (inA1 and inB1) or (((inA0 and inB0) or (inCin and (inA0 xor inB0))) and (inA1 xor inB1))
end