program GroupBy;

type
  TPerson = record
    name: string;
    age: integer;
    city: string;
  end;

  TStat = record
    city: string;
    count: integer;
    sumAge: integer;
  end;

var
  people: array[1..6] of TPerson = (
    (name:'Alice'; age:30; city:'Paris'),
    (name:'Bob'; age:15; city:'Hanoi'),
    (name:'Charlie'; age:65; city:'Paris'),
    (name:'Diana'; age:45; city:'Hanoi'),
    (name:'Eve'; age:70; city:'Paris'),
    (name:'Frank'; age:22; city:'Hanoi')
  );
  stats: array[1..6] of TStat;
  statCount: integer = 0;
  i,j: integer;
  found: boolean;
  avg: real;
begin
  for i := 1 to Length(people) do
  begin
    found := False;
    for j := 1 to statCount do
      if stats[j].city = people[i].city then
      begin
        stats[j].count := stats[j].count + 1;
        stats[j].sumAge := stats[j].sumAge + people[i].age;
        found := True;
        Break;
      end;
    if not found then
    begin
      Inc(statCount);
      stats[statCount].city := people[i].city;
      stats[statCount].count := 1;
      stats[statCount].sumAge := people[i].age;
    end;
  end;

  Writeln('--- People grouped by city ---');
  for i := 1 to statCount do
  begin
    avg := stats[i].sumAge / stats[i].count;
    Writeln(stats[i].city, ': count = ', stats[i].count, ', avg_age = ', avg:0:1);
  end;
end.
