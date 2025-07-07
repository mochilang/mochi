program UpdateStmt;

type
  Person = record
    name: string;
    age: integer;
    status: string;
  end;

var
  people: array[1..4] of Person = (
    (name:'Alice'; age:17; status:'minor'),
    (name:'Bob'; age:25; status:'unknown'),
    (name:'Charlie'; age:18; status:'unknown'),
    (name:'Diana'; age:16; status:'minor')
  );
  i: integer;
begin
  for i := 1 to Length(people) do
    if people[i].age >= 18 then
    begin
      people[i].status := 'adult';
      people[i].age := people[i].age + 1;
    end;

  Writeln('ok');
end.
