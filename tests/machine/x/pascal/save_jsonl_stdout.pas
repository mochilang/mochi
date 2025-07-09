program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic procedure _saveJSONL<T>(data: specialize TArray<T>; path: string);

var sl: TStringList;
  i: Integer;
  ds: TJSONStreamer;
begin
  sl := TStringList.Create;
  ds := TJSONStreamer.Create(nil);
  try
    for i := 0 to High(data) do
      begin
        sl.Add(ds.ObjectToJSONString(@data[i], TypeInfo(T)));
      end;
    sl.SaveToFile(path);
  finally
    ds.Free;
    sl.Free;
end;
end;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  people: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp0.AddOrSetData('age', 30);
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp1.AddOrSetData('age', 25);
  people := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1]);
  _saveJSONL(people, "-");
end.
