program GroupByHaving;

type
  TPerson = record
    name: string;
    city: string;
  end;

  TGroup = record
    city: string;
    count: integer;
  end;

var
  people: array[1..7] of TPerson = (
    (name:'Alice'; city:'Paris'),
    (name:'Bob'; city:'Hanoi'),
    (name:'Charlie'; city:'Paris'),
    (name:'Diana'; city:'Hanoi'),
    (name:'Eve'; city:'Paris'),
    (name:'Frank'; city:'Hanoi'),
    (name:'George'; city:'Paris')
  );
  groups: array[1..7] of TGroup;
  count: integer = 0;
  i,j: integer;
  found: boolean;
begin
  for i := 1 to Length(people) do
  begin
    found := False;
    for j := 1 to count do
      if groups[j].city = people[i].city then
      begin
        groups[j].count := groups[j].count + 1;
        found := True;
        Break;
      end;
    if not found then
    begin
      Inc(count);
      groups[count].city := people[i].city;
      groups[count].count := 1;
    end;
  end;

  for i := 1 to count do
    if groups[i].count >= 4 then
      Writeln(groups[i].city, ' ', groups[i].count);
end.
