program GroupByConditionalSum;

type
  TItem = record
    cat: string;
    val: integer;
    flag: boolean;
  end;

  TStat = record
    cat: string;
    sumTrue: integer;
    sumVal: integer;
  end;

var
  items: array[1..3] of TItem = (
    (cat:'a'; val:10; flag:true),
    (cat:'a'; val:5; flag:false),
    (cat:'b'; val:20; flag:true)
  );
  groups: array[1..3] of TStat;
  count: integer = 0;
  i, j: integer;
  found: boolean;
  share: real;
begin
  for i := 1 to Length(items) do
  begin
    found := False;
    for j := 1 to count do
      if groups[j].cat = items[i].cat then
      begin
        if items[i].flag then
          groups[j].sumTrue := groups[j].sumTrue + items[i].val;
        groups[j].sumVal := groups[j].sumVal + items[i].val;
        found := True;
        Break;
      end;
    if not found then
    begin
      Inc(count);
      groups[count].cat := items[i].cat;
      if items[i].flag then
        groups[count].sumTrue := items[i].val
      else
        groups[count].sumTrue := 0;
      groups[count].sumVal := items[i].val;
    end;
  end;

  for i := 1 to count do
  begin
    if groups[i].sumVal <> 0 then
      share := groups[i].sumTrue / groups[i].sumVal
    else
      share := 0;
    Writeln('cat=', groups[i].cat, ' share=', share:0:6);
  end;
end.
