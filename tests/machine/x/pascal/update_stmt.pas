program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

type Person = record
  name: string;
  age: integer;
  status: string;
end;

var
  _tmp0: Person;
  _tmp1: Person;
  _tmp2: Person;
  _tmp3: Person;
  _tmp4: integer;
  _tmp5: Person;
  people: specialize TArray<Person>;

procedure test_update_adult_status;

var
  _tmp6: Person;
  _tmp7: Person;
  _tmp8: Person;
  _tmp9: Person;
begin
  _tmp6.name := 'Alice';
  _tmp6.age := 17;
  _tmp6.status := 'minor';
  _tmp7.name := 'Bob';
  _tmp7.age := 26;
  _tmp7.status := 'adult';
  _tmp8.name := 'Charlie';
  _tmp8.age := 19;
  _tmp8.status := 'adult';
  _tmp9.name := 'Diana';
  _tmp9.age := 16;
  _tmp9.status := 'minor';
  if not ((people = specialize TArray<Person>([_tmp6, _tmp7, _tmp8, _tmp9]))) then raise Exception.
    Create('expect failed');
end;

begin
  _tmp0.name := 'Alice';
  _tmp0.age := 17;
  _tmp0.status := 'minor';
  _tmp1.name := 'Bob';
  _tmp1.age := 25;
  _tmp1.status := 'unknown';
  _tmp2.name := 'Charlie';
  _tmp2.age := 18;
  _tmp2.status := 'unknown';
  _tmp3.name := 'Diana';
  _tmp3.age := 16;
  _tmp3.status := 'minor';
  people := specialize TArray<Person>([_tmp0, _tmp1, _tmp2, _tmp3]);
  for _tmp4 := 0 to High(people) do
    begin
      _tmp5 := people[_tmp4];
      if (_tmp5.age >= 18) then
        begin
          _tmp5.status := 'adult';
          _tmp5.age := _tmp5.age + 1;
        end;
      people[_tmp4] := _tmp5;
    end;
  writeln('ok');
  test_update_adult_status;
end.
