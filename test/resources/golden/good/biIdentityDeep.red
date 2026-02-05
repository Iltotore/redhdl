component Identity3
input
  in: Boolean
output
  out: Boolean
begin
  out = in
end

component Identity2
input
  in: Boolean
output
  out: Boolean
subcomponent
  id1: Identity3
begin
  id1.in = not in
  out = not id1.out
end

component Identity
input
  in: Boolean
output
  out: Boolean
subcomponent
  id1: Identity2
begin
  id1.in = in
  out = id1.out
end

component BiIdentityDeep
input
  in1: Boolean,
  in2: Boolean
output
  out1: Boolean,
  out2: Boolean
subcomponent
  id1: Identity,
  id2: Identity
begin
  id1.in = in1
  id2.in = in2

  out1 = id1.out and (true or false)
  out2 = id2.out or (true and false)
end