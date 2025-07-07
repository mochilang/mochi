program ShortCircuit;

function boom(a, b: integer): boolean;
begin
  Writeln('boom');
  boom := True;
end;

var
  res: boolean;
begin
  if False then
    res := boom(1,2)
  else
    res := False;
  Writeln(res);

  if True then
    res := True
  else
    res := boom(1,2);
  Writeln(res);
end.
