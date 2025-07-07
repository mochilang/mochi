program NestedFunction;

function outer(x: integer): integer;
  function inner(y: integer): integer;
  begin
    inner := x + y;
  end;
begin
  outer := inner(5);
end;

begin
  Writeln(outer(3));
end.
