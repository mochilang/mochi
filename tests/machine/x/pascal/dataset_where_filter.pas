program DatasetWhereFilter;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TFPGMap<string, Variant>;
  _tmp4: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp5: specialize TFPGMap<string, Variant>;
  _tmp6: integer;
  adults: specialize TArray<specialize TFPGMap<string, Variant>>;
  people: specialize TArray<specialize TFPGMap<string, Variant>>;
  person: specialize TFPGMap<string, Variant>;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp0.AddOrSetData('age', 30);
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp1.AddOrSetData('age', 15);
  _tmp2 := specialize TFPGMap<string, Variant>.Create;
  _tmp2.AddOrSetData('name', 'Charlie');
  _tmp2.AddOrSetData('age', 65);
  _tmp3 := specialize TFPGMap<string, Variant>.Create;
  _tmp3.AddOrSetData('name', 'Diana');
  _tmp3.AddOrSetData('age', 45);
  people := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2, _tmp3]);
  SetLength(_tmp4, 0);
  for person in people do
    begin
      if not ((person.KeyData['age'] >= 18)) then continue;
      _tmp5 := specialize TFPGMap<string, Variant>.Create;
      _tmp5.AddOrSetData('name', person.KeyData['name']);
      _tmp5.AddOrSetData('age', person.KeyData['age']);
      _tmp5.AddOrSetData('is_senior', (person.KeyData['age'] >= 60));
      _tmp4 := Concat(_tmp4, [_tmp5]);
    end;
  adults := _tmp4;
  writeln('--- Adults ---');
  for person in adults do
    begin
      if person.KeyData['is_senior'] then
        begin
          _tmp6 := ' (senior)';
        end
      else
        begin
          _tmp6 := '';
        end;
      writeln(person.KeyData['name'], ' ', 'is', ' ', person.KeyData['age'], ' ', _tmp6);
    end;
end.
