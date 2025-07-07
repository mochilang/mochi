program MatchExpr;
var
  x: integer;
  labelStr: string;
begin
  x := 2;
  case x of
    1: labelStr := 'one';
    2: labelStr := 'two';
    3: labelStr := 'three';
  else
    labelStr := 'unknown';
  end;
  Writeln(labelStr);
end.
