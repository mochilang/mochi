program GroupItemsIteration;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

  generic _Group<T> = record
    Key: Variant;
    Items: specialize TArray<T>;
  end;

  generic function _appendList<T>(arr: specialize TArray<T>; val: T): specialize TArray<T>;

var i,n: Integer;
begin
  n := Length(arr);
  SetLength(Result, n + 1);
  for i := 0 to n - 1 do
    Result[i] := arr[i];
  Result[n] := val;
end;

generic function _group_by<T>(src: specialize TArray<T>; keyfn: function(it: T): Variant):
                                                                                          specialize
                                                                                           TArray<
                                                                                          specialize
                                                                                           _Group<T>
                                                                                           >;

var i,j,idx: Integer;
  key: Variant;
  ks: string;
begin
  SetLength(Result, 0);
  for i := 0 to High(src) do
    begin
      key := keyfn(src[i]);
      ks := VarToStr(key);
      idx := -1;
      for j := 0 to High(Result) do
        if VarToStr(Result[j].Key) = ks then
          begin
            idx := j;
            Break;
          end;
      if idx = -1 then
        begin
          idx := Length(Result);
          SetLength(Result, idx + 1);
          Result[idx].Key := key;
          SetLength(Result[idx].Items, 0);
        end;
      SetLength(Result[idx].Items, Length(Result[idx].Items)+1);
      Result[idx].Items[High(Result[idx].Items)] := src[i];
    end;
end;

generic procedure _printList<T>(arr: specialize TArray<T>);

var i: Integer;
begin
  for i := 0 to High(arr) do
    begin
      if i > 0 then Write(' ');
      Write(arr[i]);
    end;
  writeln();
end;

generic procedure _sortBy<T>(var arr: specialize TArray<T>; keys: specialize TArray<Variant>);

var i,j: integer;
  tmp: T;
  k: Variant;
begin
  for i := 0 to High(arr) - 1 do
    for j := i + 1 to High(arr) do
      if keys[i] > keys[j] then
        begin
          tmp := arr[i];
          arr[i] := arr[j];
          arr[j] := tmp;
          k := keys[i];
          keys[i] := keys[j];
          keys[j] := k;
        end;
end;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp4: specialize TArray<specialize _Group<specialize TFPGMap<string, Variant>>>;
  _tmp5: specialize TArray<Variant>;
  _tmp6: specialize TFPGMap<string, Variant>;
  _tmp7: specialize TArray<Variant>;
  _tmp8: specialize TArray<Variant>;
  d: specialize TFPGMap<string, Variant>;
  data: specialize TArray<specialize TFPGMap<string, Variant>>;
  g: Variant;
  groups: specialize TArray<Variant>;
  r: Variant;
  _result: specialize TArray<Variant>;
  tmp: specialize TArray<Variant>;
  total: integer;
  x: Variant;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('tag', 'a');
  _tmp0.AddOrSetData('val', 1);
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('tag', 'a');
  _tmp1.AddOrSetData('val', 2);
  _tmp2 := specialize TFPGMap<string, Variant>.Create;
  _tmp2.AddOrSetData('tag', 'b');
  _tmp2.AddOrSetData('val', 3);
  data := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2]);
  SetLength(_tmp3, 0);
  for d in data do
    begin
      _tmp3 := Concat(_tmp3, [d]);
    end;
  _tmp4 := specialize _group_by<specialize TFPGMap<string, Variant>>(_tmp3, function(d: specialize
           TFPGMap<string, Variant>): Variant begin Result := d.KeyData['tag']
end
);
SetLength(_tmp5, 0);
for g in _tmp4 do
  begin
    _tmp5 := Concat(_tmp5, [g]);
  end;
groups := _tmp5;
tmp := specialize TArray<Variant>([]);
for g in groups do
  begin
    total := 0;
    for x in g.items do
      begin
        total := total + x.val;
      end;
    _tmp6 := specialize TFPGMap<string, Variant>.Create;
    _tmp6.AddOrSetData('tag', g.key);
    _tmp6.AddOrSetData('total', total);
    tmp := specialize _appendList<Variant>(tmp, _tmp6);
  end;
SetLength(_tmp7, 0);
SetLength(_tmp8, 0);
for r in tmp do
  begin
    _tmp7 := Concat(_tmp7, [r]);
    _tmp8 := Concat(_tmp8, [r.tag]);
  end;
specialize _sortBy<Variant>(_tmp7, _tmp8);
_result := _tmp7;
specialize _printList<Variant>(_result);
end.
