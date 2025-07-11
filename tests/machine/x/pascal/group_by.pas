program GroupBy;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

  generic _Group<T> = record
    Key: Variant;
    Items: specialize TArray<T>;
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

generic function _avgList<T>(arr: specialize TArray<T>): double;
begin
  if Length(arr) = 0 then exit(0);
  Result := specialize _sumList<T>(arr) / Length(arr);
end;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, Variant>;
  _tmp10: specialize TArray<Variant>;
  _tmp2: specialize TFPGMap<string, Variant>;
  _tmp3: specialize TFPGMap<string, Variant>;
  _tmp4: specialize TFPGMap<string, Variant>;
  _tmp5: specialize TFPGMap<string, Variant>;
  _tmp6: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp7: specialize TArray<specialize _Group<specialize TFPGMap<string, Variant>>>;
  _tmp8: specialize TArray<_>;
  _tmp9: specialize TFPGMap<string, Variant>;
  p: integer;
  people: specialize TArray<specialize TFPGMap<string, Variant>>;
  person: specialize TFPGMap<string, Variant>;
  s: specialize TFPGMap<string, Variant>;
  stats: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp0.AddOrSetData('age', 30);
  _tmp0.AddOrSetData('city', 'Paris');
  _tmp1 := specialize TFPGMap<string, Variant>.Create;
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp1.AddOrSetData('age', 15);
  _tmp1.AddOrSetData('city', 'Hanoi');
  _tmp2 := specialize TFPGMap<string, Variant>.Create;
  _tmp2.AddOrSetData('name', 'Charlie');
  _tmp2.AddOrSetData('age', 65);
  _tmp2.AddOrSetData('city', 'Paris');
  _tmp3 := specialize TFPGMap<string, Variant>.Create;
  _tmp3.AddOrSetData('name', 'Diana');
  _tmp3.AddOrSetData('age', 45);
  _tmp3.AddOrSetData('city', 'Hanoi');
  _tmp4 := specialize TFPGMap<string, Variant>.Create;
  _tmp4.AddOrSetData('name', 'Eve');
  _tmp4.AddOrSetData('age', 70);
  _tmp4.AddOrSetData('city', 'Paris');
  _tmp5 := specialize TFPGMap<string, Variant>.Create;
  _tmp5.AddOrSetData('name', 'Frank');
  _tmp5.AddOrSetData('age', 22);
  _tmp5.AddOrSetData('city', 'Hanoi');
  people := specialize TArray<specialize TFPGMap<string, Variant>>([_tmp0, _tmp1, _tmp2, _tmp3,
            _tmp4, _tmp5]);
  SetLength(_tmp6, 0);
  for person in people do
    begin
      _tmp6 := Concat(_tmp6, [person]);
    end;
  _tmp7 := specialize _group_by<specialize TFPGMap<string, Variant>>(_tmp6, function(person:
           specialize TFPGMap<string, Variant>): Variant begin Result := person.KeyData['city']
end
);
SetLength(_tmp8, 0);
for g in _tmp7 do
  begin
    _tmp9 := specialize TFPGMap<string, Variant>.Create;
    _tmp9.AddOrSetData('city', g.key);
    _tmp9.AddOrSetData('count', Length(g));
    SetLength(_tmp10, 0);
    for p in g do
      begin
        _tmp10 := Concat(_tmp10, [p.age]);
      end;
    _tmp9.AddOrSetData('avg_age', specialize _avgList<Variant>(_tmp10));
    _tmp8 := Concat(_tmp8, [_tmp9]);
  end;
stats := _tmp8;
writeln('--- People grouped by city ---');
for s in stats do
  begin
    writeln(s.KeyData['city'], ' ', ': count =', ' ', s.KeyData['count'], ' ', ', avg_age =', ' ', s
            .KeyData['avg_age']);
  end;
end.
