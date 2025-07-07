program OrderByMap;

type
  TData = record
    a: integer;
    b: integer;
  end;

var
  data: array[1..3] of TData = (
    (a:1; b:2),
    (a:1; b:1),
    (a:0; b:5)
  );
  i,j: integer;
  tmp: TData;
begin
  { simple sort by a then b }
  for i := 1 to High(data)-1 do
    for j := i+1 to High(data) do
      if (data[j].a < data[i].a) or
         ((data[j].a = data[i].a) and (data[j].b < data[i].b)) then
      begin
        tmp := data[i];
        data[i] := data[j];
        data[j] := tmp;
      end;

  for i := 1 to Length(data) do
    Writeln('a=', data[i].a, ' b=', data[i].b);
end.
