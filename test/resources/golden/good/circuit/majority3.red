component Majority3
input
  inA: Boolean,
  inB: Boolean,
  inC: Boolean
output
  out: Boolean
begin
  out = (inA and inB) or (inA and inC) or (inB and inC)
end