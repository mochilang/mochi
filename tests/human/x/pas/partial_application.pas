program PartialApplication;

function add(a, b: integer): integer;
begin
  add := a + b;
end;

function add5(b: integer): integer;
begin
  add5 := add(5, b);
end;

begin
  Writeln(add5(3));
end.
