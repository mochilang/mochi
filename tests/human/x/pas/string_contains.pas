program StringContains;
var
  s: string;
begin
  s := 'catch';
  Writeln(Pos('cat', s) > 0);
  Writeln(Pos('dog', s) > 0);
end.
