component And
input
  inA: Boolean,
  inB: Boolean
output
  out: Boolean
subcomponent
  notA: Not,
  notB: Not,
  notRes: Not,
  _or: Or
begin
  notA.in = inA
  notB.in = inB
  _or.inA = notA.out
  _or.inB = notB.out
  notRes.in = _or.out
  out = notRes.out
end