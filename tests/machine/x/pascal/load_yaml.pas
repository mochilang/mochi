program main;
{$mode objfpc}

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
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TArray<specialize TFPGMap<string, string>>;
  a: specialize TFPGMap<string, string>;
  adults: specialize TArray<specialize TFPGMap<string, string>>;
  p: Person;
  people: specialize TArray<Person>;

begin
  people := _load("../interpreter/valid/people.yaml");
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('name', p.name);
  _tmp0.AddOrSetData('email', p.email);
  SetLength(_tmp1, 0);
  for p in people do
    begin
      if not ((p.age >= 18)) then continue;
      _tmp1 := Concat(_tmp1, [_tmp0]);
    end;
  adults := _tmp1;
  for a in adults do
    begin
      writeln(a.name, ' ', a.email);
    end;
end.
