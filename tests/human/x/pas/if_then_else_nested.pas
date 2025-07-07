program IfThenElseNested;
var
  x: integer;
  msg: string;
begin
  x := 8;
  if x > 10 then
    msg := 'big'
  else if x > 5 then
    msg := 'medium'
  else
    msg := 'small';
  Writeln(msg);
end.
