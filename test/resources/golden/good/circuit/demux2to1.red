component Demux1to2
input
  sel: Boolean,
  in: Boolean
output
  out0: Boolean,
  out1: Boolean
begin
  out0 = (not sel) and in
  out1 = sel and in
end