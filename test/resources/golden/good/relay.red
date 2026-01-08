component Relay
input
  inA: Boolean,
  inB: Boolean,
  inC: Boolean
output
  out: Boolean
begin
  out = (inA and inC) or inB
end