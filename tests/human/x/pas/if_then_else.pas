program IfThenElse;
var
  x: integer;
  msg: string;
begin
  x := 12;
  if x > 10 then
    msg := 'yes'
  else
    msg := 'no';
  Writeln(msg);
end.
