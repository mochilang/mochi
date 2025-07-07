program FunThreeArgs;

function Sum3(a,b,c: integer): integer;
begin
  Sum3 := a + b + c;
end;

begin
  Writeln(Sum3(1,2,3));
end.
