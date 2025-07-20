{$mode objfpc}
program Main;
var
  matrix: array of integer;
begin
  matrix := [[1, 2], [3, 4]];
  matrix[1][0] := 5;
  writeln(matrix[1][0]);
end.
