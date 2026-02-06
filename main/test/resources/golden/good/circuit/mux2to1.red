component Mux2to1
input
  sel: Boolean,
  inA: Boolean,
  inB: Boolean
output
  out: Boolean
begin
  out = (not sel and inA) or (sel and inB)
end