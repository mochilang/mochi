program ExistsBuiltin;
var
  data: array[1..2] of integer = (1,2);
  flag: boolean;
  i: integer;
begin
  flag := False;
  for i := 1 to Length(data) do
    if data[i] = 1 then
    begin
      flag := True;
      break;
    end;
  Writeln(flag);
end.
