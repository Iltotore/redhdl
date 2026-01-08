component Parity4
input
  inA: Boolean,
  inB: Boolean,
  inC: Boolean,
  inD: Boolean
output
  odd: Boolean
begin
  odd = inA xor inB xor inC xor inD
end