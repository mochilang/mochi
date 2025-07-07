program GroupItemsIteration;

type
  TData = record
    tag: string;
    val: integer;
  end;

  TGroup = record
    tag: string;
    total: integer;
  end;

var
  data: array[1..3] of TData = (
    (tag:'a'; val:1),
    (tag:'a'; val:2),
    (tag:'b'; val:3)
  );
  groups: array[1..2] of TGroup;
  count: integer = 0;
  i,j: integer;
  found: boolean;
  tmp: TGroup;
begin
  for i := 1 to Length(data) do
  begin
    found := False;
    for j := 1 to count do
      if groups[j].tag = data[i].tag then
      begin
        groups[j].total := groups[j].total + data[i].val;
        found := True;
        Break;
      end;
    if not found then
    begin
      Inc(count);
      groups[count].tag := data[i].tag;
      groups[count].total := data[i].val;
    end;
  end;

  { sort by tag }
  for i := 1 to count-1 do
    for j := i+1 to count do
      if groups[j].tag < groups[i].tag then
      begin
        tmp := groups[i];
        groups[i] := groups[j];
        groups[j] := tmp;
      end;

  for i := 1 to count do
    Writeln(groups[i].tag, ' ', groups[i].total);
end.
