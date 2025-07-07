program PureGlobalFold;

const
  k = 2;

function incVal(x: integer): integer;
begin
  incVal := x + k;
end;

begin
  Writeln(incVal(3));
end.
