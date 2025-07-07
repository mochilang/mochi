program SortStable;

type
  TItem = record
    n: integer;
    v: string;
  end;

var
  items: array[1..3] of TItem;
  i,j: integer;
  temp: TItem;
begin
  items[1].n := 1; items[1].v := 'a';
  items[2].n := 1; items[2].v := 'b';
  items[3].n := 2; items[3].v := 'c';

  for i := 1 to 2 do
    for j := i+1 to 3 do
      if items[i].n > items[j].n then
      begin
        temp := items[i];
        items[i] := items[j];
        items[j] := temp;
      end;

  for i := 1 to 3 do
  begin
    if i > 1 then Write(' ');
    Write(items[i].v);
  end;
  Writeln;
end.
