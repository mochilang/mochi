program LoadYaml;

type
  TPerson = record
    name: string;
    age: integer;
    email: string;
  end;

var
  people: array[1..3] of TPerson = (
    (name:'Alice'; age:30; email:'alice@example.com'),
    (name:'Bob'; age:15; email:'bob@example.com'),
    (name:'Charlie'; age:20; email:'charlie@example.com')
  );
  i: integer;
begin
  for i := 1 to Length(people) do
    if people[i].age >= 18 then
      Writeln(people[i].name, ' ', people[i].email);
end.
