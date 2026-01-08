component PriorityEncoder4to2
input
  d0: Boolean,
  d1: Boolean,
  d2: Boolean,
  d3: Boolean
output
  y1: Boolean,
  y0: Boolean,
  valid: Boolean
begin
  valid = d0 or d1 or d2 or d3
  y1 = d2 or d3
  y0 = d3 or (d1 and (not d2) and (not d3))
end