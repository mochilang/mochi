program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

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

generic procedure _json<T>(v: T);
begin
  writeln('[]');
end;

var
  _tmp0: specialize TFPGMap<Variant, Variant>;
  _tmp1: specialize TFPGMap<Variant, Variant>;
  _tmp10: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp2: specialize TFPGMap<Variant, Variant>;
  _tmp3: specialize TFPGMap<Variant, Variant>;
  _tmp4: specialize TFPGMap<Variant, Variant>;
  _tmp5: specialize TFPGMap<Variant, Variant>;
  _tmp6: specialize TFPGMap<Variant, Variant>;
  _tmp7: specialize TFPGMap<Variant, Variant>;
  _tmp8: specialize TArray<specialize TFPGMap<string, string>>;
  _tmp9: specialize TArray<specialize _Group<specialize TFPGMap<string, string>>>;
  big: specialize TArray<specialize TFPGMap<string, Variant>>;
  p: specialize TFPGMap<string, string>;
  people: specialize TArray<specialize TFPGMap<string, string>>;

begin
  _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp0.AddOrSetData('name', 'Alice');
  _tmp0.AddOrSetData('city', 'Paris');
  _tmp1 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp1.AddOrSetData('name', 'Bob');
  _tmp1.AddOrSetData('city', 'Hanoi');
  _tmp2 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp2.AddOrSetData('name', 'Charlie');
  _tmp2.AddOrSetData('city', 'Paris');
  _tmp3 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp3.AddOrSetData('name', 'Diana');
  _tmp3.AddOrSetData('city', 'Hanoi');
  _tmp4 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp4.AddOrSetData('name', 'Eve');
  _tmp4.AddOrSetData('city', 'Paris');
  _tmp5 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp5.AddOrSetData('name', 'Frank');
  _tmp5.AddOrSetData('city', 'Hanoi');
  _tmp6 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp6.AddOrSetData('name', 'George');
  _tmp6.AddOrSetData('city', 'Paris');
  people := specialize TArray<specialize TFPGMap<string, string>>([_tmp0, _tmp1, _tmp2, _tmp3, _tmp4
            , _tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<Variant, Variant>.Create;
  _tmp7.AddOrSetData('city', g.key);
  _tmp7.AddOrSetData('num', Length(g));
  SetLength(_tmp8, 0);
  for p in people do
    begin
      _tmp8 := Concat(_tmp8, [p]);
    end;
  _tmp9 := specialize _group_by<specialize TFPGMap<string, string>>(_tmp8, function(p: specialize
           TFPGMap<string, string>): Variant begin Result := p.city
end
);
SetLength(_tmp10, 0);
for g in _tmp9 do
  begin
    _tmp10 := Concat(_tmp10, [_tmp7]);
  end;
big := _tmp10;
specialize _json<specialize TArray<specialize TFPGMap<string, Variant>>>(big);
end.
