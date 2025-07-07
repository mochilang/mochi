program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic _Group<T> = record
    Key: Variant;
    Items: specialize TArray<T>;
  end;

  generic function _group_by<T>(src: specialize TArray<T>; keyfn: function(it: T): Variant):
                                                                                          specialize
                                                                                             TArray<
                                                                                          specialize
                                                                                             _Group<
                                                                                             T>>;

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

generic function _sumList<T>(arr: specialize TArray<T>): double;

var i: integer;
  s: double;
begin
  s := 0;
  for i := 0 to High(arr) do
    s := s + arr[i];
  Result := s;
end;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp10: specialize TArray<Variant>;
  _tmp11: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp12: specialize TArray<specialize _Group<specialize TFPGMap<string, Variant>>>;
  _tmp13: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp2: specialize TFPGMap<Variant, integer>;
  _tmp3: specialize TFPGMap<Variant, integer>;
  _tmp4: specialize TFPGMap<Variant, Variant>;
  _tmp5: specialize TFPGMap<Variant, Variant>;
  _tmp6: specialize TFPGMap<Variant, Variant>;
  _tmp7: specialize TFPGMap<Variant, Variant>;
  _tmp8: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp9: specialize TFPGMap<Variant, Variant>;
  filtered: specialize TArray<specialize TFPGMap<string, Variant>>;
  grouped: specialize TArray<specialize TFPGMap<string, Variant>>;
  nations: specialize TArray<specialize TFPGMap<string, Variant>>;
  partsupp: specialize TArray<specialize TFPGMap<string, Variant>>;
  ps: specialize TFPGMap<string, Variant>;
  r: integer;
  suppliers: specialize TArray<specialize TFPGMap<string, integer>>;
  x: specialize TFPGMap<string, Variant>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('id', 1);
  _tmp0.AddOrSetData('name', 'A');
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('id', 2);
  _tmp1.AddOrSetData('name', 'B');
  nations := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1]);
  _tmp2 := specialize TFPGMap<Variant, integer>.Create;
  _tmp2.AddOrSetData('id', 1);
  _tmp2.AddOrSetData('nation', 1);
  _tmp3 := specialize TFPGMap<Variant, integer>.Create;
  _tmp3.AddOrSetData('id', 2);
  _tmp3.AddOrSetData('nation', 2);
  suppliers := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp4.AddOrSetData('part', 100);
  _tmp4.AddOrSetData('supplier', 1);
  _tmp4.AddOrSetData('cost', 10);
  _tmp4.AddOrSetData('qty', 2);
  _tmp5 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp5.AddOrSetData('part', 100);
  _tmp5.AddOrSetData('supplier', 2);
  _tmp5.AddOrSetData('cost', 20);
  _tmp5.AddOrSetData('qty', 1);
  _tmp6 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp6.AddOrSetData('part', 200);
  _tmp6.AddOrSetData('supplier', 1);
  _tmp6.AddOrSetData('cost', 5);
  _tmp6.AddOrSetData('qty', 3);
  partsupp := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp4, _tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp7.AddOrSetData('part', ps.part);
  _tmp7.AddOrSetData('value', ps.cost * ps.qty);
  SetLength(_tmp8, 0);
  for ps in partsupp do
    begin
      for s in suppliers do
        begin
          if not ((s.id = ps.supplier)) then continue;
          for n in nations do
            begin
              if not ((n.id = s.nation)) then continue;
              if not ((n.name = 'A')) then continue;
              _tmp8 := Concat(_tmp8, [_tmp7]);
            end;
        end;
    end;
  filtered := _tmp8;
  _tmp9 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp9.AddOrSetData('part', g.key);
  SetLength(_tmp10, 0);
  for r in g do
    begin
      _tmp10 := Concat(_tmp10, [r.value]);
    end;
  _tmp9.AddOrSetData('total', specialize _sumList<Variant>(_tmp10));
  SetLength(_tmp11, 0);
  for x in filtered do
    begin
      _tmp11 := Concat(_tmp11, [x]);
    end;
  _tmp12 := specialize _group_by<specialize TFPGMap<string, Variant>>(_tmp11, function(x: specialize
            TFPGMap<string, Variant>): Variant begin Result := x.part
end
);
SetLength(_tmp13, 0);
for g in _tmp12 do
  begin
    _tmp13 := Concat(_tmp13, [_tmp9]);
  end;
grouped := _tmp13;
writeln(grouped);
end.
