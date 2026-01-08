component ExactlyOne3
input
  inA: Boolean,
  inB: Boolean,
  inC: Boolean
output
  out: Boolean
begin
  out = (inA and (not inB) and (not inC)) or ((not inA) and inB and (not inC)) or ((not inA) and (not inB) and inC)
end