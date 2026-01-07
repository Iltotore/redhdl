component HalfAdder
input
  inA: Boolean
  inB: Boolean
output
  sum: Boolean
  carry: Boolean
begin
  carry = inA and inB
  sum = inA xor inB
end