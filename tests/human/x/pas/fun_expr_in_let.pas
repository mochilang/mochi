program FunExprInLet;

type
  TIntFunc = function(x: integer): integer;

function SquareImpl(x: integer): integer;
begin
  SquareImpl := x * x;
end;

var
  square: TIntFunc;
begin
  square := @SquareImpl;
  Writeln(square(6));
end.
