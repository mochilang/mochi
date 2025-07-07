program GroupBySort;

type
  TItem = record
    cat: string;
    val: integer;
  end;

  TRow = record
    cat: string;
    total: integer;
  end;

var
  items: array[1..4] of TItem = (
    (cat:'a'; val:3),
    (cat:'a'; val:1),
    (cat:'b'; val:5),
    (cat:'b'; val:2)
  );
  groups: array[1..2] of TRow;
  count: integer = 0;
  i,j: integer;
  found: boolean;
  tmp: TRow;
begin
  for i := 1 to Length(items) do
  begin
    found := False;
    for j := 1 to count do
      if groups[j].cat = items[i].cat then
      begin
        groups[j].total := groups[j].total + items[i].val;
        found := True;
        Break;
      end;
    if not found then
    begin
      Inc(count);
      groups[count].cat := items[i].cat;
      groups[count].total := items[i].val;
    end;
  end;

  { sort descending by total }
  for i := 1 to count-1 do
    for j := i+1 to count do
      if groups[j].total > groups[i].total then
      begin
        tmp := groups[i];
        groups[i] := groups[j];
        groups[j] := tmp;
      end;

  for i := 1 to count do
    Writeln('cat=', groups[i].cat, ' total=', groups[i].total);
end.
