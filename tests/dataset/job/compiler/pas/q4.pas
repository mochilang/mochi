program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q4_returns_minimum_rating_and_title_for_sequels;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('rating', '6.2');
  _tmp0.AddOrSetData('_tmp0ovie_title', 'Alpha Movie');
  if not ((_result = specialize TArray<specialize TFPGMap<string, string>>([_tmp0]))) then raise Exception.Create('expect failed');
end;

var
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp10: specialize TFPGMap<string, integer>;
  _tmp11: specialize TFPGMap<string, integer>;
  _tmp12: specialize TFPGMap<string, integer>;
  _tmp13: specialize TFPGMap<string, integer>;
  _tmp14: specialize TFPGMap<string, integer>;
  _tmp15: specialize TArray<specialize TFPGMap<string, integer>>;
  _tmp16: specialize TArray<integer>;
  _tmp17: specialize TArray<integer>;
  _tmp18: specialize TFPGMap<string, integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  info_type: specialize TArray<specialize TFPGMap<string, integer>>;
  it: specialize TFPGMap<string, integer>;
  keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_info_idx: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  r: specialize TFPGMap<string, integer>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  rows: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('id', 1);
  _tmp1.AddOrSetData('info', 'rating');
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('id', 2);
  _tmp2.AddOrSetData('info', 'other');
  info_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 1);
  _tmp3.AddOrSetData('keyword', 'great sequel');
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('id', 2);
  _tmp4.AddOrSetData('keyword', 'prequel');
  keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4]);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('id', 10);
  _tmp5.AddOrSetData('title', 'Alpha Movie');
  _tmp5.AddOrSetData('production_year', 2006);
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('id', 20);
  _tmp6.AddOrSetData('title', 'Beta Fil_tmp6');
  _tmp6.AddOrSetData('production_year', 2007);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 30);
  _tmp7.AddOrSetData('title', 'Old Fil_tmp7');
  _tmp7.AddOrSetData('production_year', 2004);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp5, _tmp6, _tmp7]);
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('_tmp8ovie_id', 10);
  _tmp8.AddOrSetData('keyword_id', 1);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('_tmp9ovie_id', 20);
  _tmp9.AddOrSetData('keyword_id', 1);
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('_tmp10ovie_id', 30);
  _tmp10.AddOrSetData('keyword_id', 1);
  movie_keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp8, _tmp9, _tmp10]);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('_tmp11ovie_id', 10);
  _tmp11.AddOrSetData('info_type_id', 1);
  _tmp11.AddOrSetData('info', '6.2');
  _tmp12 := specialize TFPGMap<string, integer>.Create;
  _tmp12.AddOrSetData('_tmp12ovie_id', 20);
  _tmp12.AddOrSetData('info_type_id', 1);
  _tmp12.AddOrSetData('info', '7.8');
  _tmp13 := specialize TFPGMap<string, integer>.Create;
  _tmp13.AddOrSetData('_tmp13ovie_id', 30);
  _tmp13.AddOrSetData('info_type_id', 1);
  _tmp13.AddOrSetData('info', '4.5');
  movie_info_idx := specialize TArray<specialize TFPGMap<string, integer>>([_tmp11, _tmp12, _tmp13]);
  _tmp14 := specialize TFPGMap<string, integer>.Create;
  _tmp14.AddOrSetData('rating', _tmp14i.info);
  _tmp14.AddOrSetData('title', t.title);
  SetLength(_tmp15, 0);
  for it in info_type do
  begin
    for mi in movie_info_idx do
    begin
      if not ((it.id = mi.info_type_id)) then continue;
      for t in title do
      begin
        if not ((t.id = mi.movie_id)) then continue;
        for mk in movie_keyword do
        begin
          if not ((mk.movie_id = t.id)) then continue;
          for k in keyword do
          begin
            if not ((k.id = mk.keyword_id)) then continue;
            if not ((((((it.info = 'rating') and k.keyword.contains('sequel')) and (mi.info > '5.0')) and (t.production_year > 2005)) and (mk.movie_id = mi.movie_id))) then continue;
            _tmp15 := Concat(_tmp15, [_tmp14]);
          end;
        end;
      end;
    end;
  end;
  rows := _tmp15;
  SetLength(_tmp16, 0);
  for r in rows do
  begin
    _tmp16 := Concat(_tmp16, [r.rating]);
  end;
  SetLength(_tmp17, 0);
  for r in rows do
  begin
    _tmp17 := Concat(_tmp17, [r.title]);
  end;
  _tmp18 := specialize TFPGMap<string, integer>.Create;
  _tmp18.AddOrSetData('rating', _tmp18in(_t_tmp18p16));
  _tmp18.AddOrSetData('_tmp18ovie_title', _tmp18in(_t_tmp18p17));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp18]);
  json(_result);
  test_Q4_returns_minimum_rating_and_title_for_sequels;
end.
