program LoadYaml;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

type Person = record
  name: string;
  age: integer;
  email: string;
end;

generic function _loadYAML<T>(path: string): specialize TArray<T>;

var sl: TStringList;
  data: TJSONData;
  arr: TJSONArray;
  i: Integer;
  ds: TJSONDeStreamer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(path);
    data := GetJSON(sl.Text);
    arr := TJSONArray(data);
    SetLength(Result, arr.Count);
    ds := TJSONDeStreamer.Create(nil);
    try
      for i := 0 to arr.Count - 1 do
        ds.JSONToObject(arr.Objects[i], @Result[i], TypeInfo(T));
    finally
      ds.Free;
end;
finally
  sl.Free;
end;
end;

var
  _tmp0: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp1: specialize TFPGMap<string, Variant>;
  a: specialize TFPGMap<string, Variant>;
  adults: specialize TArray<specialize TFPGMap<string, Variant>>;
  p: Person;
  people: specialize TArray<Person>;

begin
  people := specialize _loadYAML<Person>("../interpreter/valid/people.yaml");
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
