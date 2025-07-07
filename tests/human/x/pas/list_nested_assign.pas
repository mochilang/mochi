program ListNestedAssign;
var
  matrix: array[0..1,0..1] of integer;
begin
  matrix[0,0] := 1;
  matrix[0,1] := 2;
  matrix[1,0] := 3;
  matrix[1,1] := 4;
  matrix[1,0] := 5;
  Writeln(matrix[1,0]);
end.
