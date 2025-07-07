program CastStringToInt;
var
  s: string;
  n: integer;
begin
  s := '1995';
  Val(s, n);
  Writeln(n);
end.
