{$mode objfpc}
program Main;
var
  result: string;
  i: integer;
  j: integer;
begin
  result := '';
  for i := 1 to (101 - 1) do begin
  j := 1;
  while (j * j) < i do begin
  j := j + 1;
end;
  if (j * j) = i then begin
  result := result + 'O';
end else begin
  result := result + '-';
end;
end;
  writeln(result);
end.
