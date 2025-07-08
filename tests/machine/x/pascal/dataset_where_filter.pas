program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, Variant>;
  _tmp4: specialize TFPGMap<Variant, Variant>;
  _tmp5: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp6: integer;
  adults: specialize TArray<specialize TFPGMap<string, Variant>>;
  people: specialize TArray<specialize TFPGMap<string, Variant>>;
  person: specialize TFPGMap<string, Variant>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp0.AddOrSetData('age', 30);
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp1.AddOrSetData('age', 15);
  _tmp2 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp2.AddOrSetData('name', 'Charlie');
  _tmp2.AddOrSetData('age', 65);
  _tmp3 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp3.AddOrSetData('name', 'Diana');
  _tmp3.AddOrSetData('age', 45);
  people := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp4.AddOrSetData('name', person.name);
  _tmp4.AddOrSetData('age', person.age);
  _tmp4.AddOrSetData('is_senior', (person.age >= 60));
  SetLength(_tmp5, 0);
  for person in people do
    begin
      if not ((person.age >= 18)) then continue;
      _tmp5 := Concat(_tmp5, [_tmp4]);
    end;
  adults := _tmp5;
  writeln('--- Adults ---');
  for person in adults do
    begin
      if person.is_senior then
        begin
          _tmp6 := ' (senior)';
        end
      else
        begin
          _tmp6 := '';
        end;
      writeln(person.name, ' ', 'is', ' ', person.age, ' ', _tmp6);
    end;
end.
