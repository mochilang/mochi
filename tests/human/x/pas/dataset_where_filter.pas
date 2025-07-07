program DatasetWhereFilter;

type
  Person = record
    name: string;
    age: Integer;
  end;

const
  PeopleCount = 4;
var
  people: array[1..PeopleCount] of Person;
  i: Integer;
begin
  people[1].name := 'Alice'; people[1].age := 30;
  people[2].name := 'Bob'; people[2].age := 15;
  people[3].name := 'Charlie'; people[3].age := 65;
  people[4].name := 'Diana'; people[4].age := 45;

  Writeln('--- Adults ---');
  for i := 1 to PeopleCount do
  begin
    if people[i].age >= 18 then
    begin
      write(people[i].name, ' is ', people[i].age);
      if people[i].age >= 60 then
        Writeln(' (senior)')
      else
        Writeln;
    end;
  end;
end.
