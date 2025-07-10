program LoadYaml;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

type Person = record
  name: string;
  age: integer;
  email: string;
end;

function _load(path: string): specialize TArray<string>;

var sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(path);
    SetLength(Result, sl.Count);
    for i := 0 to sl.Count - 1 do
      Result[i] := sl[i];
  finally
    sl.Free;
end;
end;

var
  _tmp0: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp1: specialize TFPGMap<string, Variant>;
  a: specialize TFPGMap<string, string>;
  adults: specialize TArray<specialize TFPGMap<string, string>>;
  p: Person;
  people: specialize TArray<Person>;

begin
  people := _load("../interpreter/valid/people.yaml");
  SetLength(_tmp0, 0);
  for p in people do
    begin
      if not ((p.age >= 18)) then continue;
      _tmp1 := specialize TFPGMap<string, Variant>.Create;
      _tmp1.AddOrSetData('name', p.name);
      _tmp1.AddOrSetData('email', p.email);
      _tmp0 := Concat(_tmp0, [_tmp1]);
    end;
  adults := _tmp0;
  for a in adults do
    begin
      writeln(a.KeyData['name'], ' ', a.KeyData['email']);
    end;
end.
