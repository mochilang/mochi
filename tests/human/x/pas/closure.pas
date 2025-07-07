program Closure;

type
  TAdder = function(x: integer): integer;

function makeAdder(n: integer): TAdder;
  function add(x: integer): integer;
  begin
    add := x + n;
  end;
begin
  makeAdder := @add;
end;

var
  add10: TAdder;
begin
  add10 := makeAdder(10);
  Writeln(add10(7));
end.
