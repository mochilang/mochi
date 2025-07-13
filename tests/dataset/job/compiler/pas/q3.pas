program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q3_returns_lexicographically_smallest_sequel_title;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('_tmp0ovie_title', 'Alpha');
  if not ((_result = specialize TArray<specialize TFPGMap<string, string>>([_tmp0]))) then raise Exception.Create('expect failed');
end;

var
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp10: specialize TFPGMap<string, integer>;
  _tmp11: specialize TFPGMap<string, integer>;
  _tmp12: specialize TFPGMap<string, integer>;
  _tmp13: specialize TArray<integer>;
  _tmp14: specialize TFPGMap<string, integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  allowed_infos: specialize TArray<string>;
  candidate_titles: specialize TArray<integer>;
  k: specialize TFPGMap<string, integer>;
  keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_info: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('id', 1);
  _tmp1.AddOrSetData('keyword', 'a_tmp1azing sequel');
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('id', 2);
  _tmp2.AddOrSetData('keyword', 'prequel');
  keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('_tmp3ovie_id', 10);
  _tmp3.AddOrSetData('info', 'Ger_tmp3any');
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('_tmp4ovie_id', 30);
  _tmp4.AddOrSetData('info', 'Sweden');
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('_tmp5ovie_id', 20);
  _tmp5.AddOrSetData('info', 'France');
  movie_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4, _tmp5]);
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('_tmp6ovie_id', 10);
  _tmp6.AddOrSetData('keyword_id', 1);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('_tmp7ovie_id', 30);
  _tmp7.AddOrSetData('keyword_id', 1);
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('_tmp8ovie_id', 20);
  _tmp8.AddOrSetData('keyword_id', 1);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('_tmp9ovie_id', 10);
  _tmp9.AddOrSetData('keyword_id', 2);
  movie_keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp6, _tmp7, _tmp8, _tmp9]);
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('id', 10);
  _tmp10.AddOrSetData('title', 'Alpha');
  _tmp10.AddOrSetData('production_year', 2006);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('id', 30);
  _tmp11.AddOrSetData('title', 'Beta');
  _tmp11.AddOrSetData('production_year', 2008);
  _tmp12 := specialize TFPGMap<string, integer>.Create;
  _tmp12.AddOrSetData('id', 20);
  _tmp12.AddOrSetData('title', 'Ga_tmp12_tmp12a');
  _tmp12.AddOrSetData('production_year', 2009);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp10, _tmp11, _tmp12]);
  allowed_infos := specialize TArray<string>(['Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German']);
  SetLength(_tmp13, 0);
  for k in keyword do
  begin
    for mk in movie_keyword do
    begin
      if not ((mk.keyword_id = k.id)) then continue;
      for mi in movie_info do
      begin
        if not ((mi.movie_id = mk.movie_id)) then continue;
        for t in title do
        begin
          if not ((t.id = mi.movie_id)) then continue;
          if not ((((k.keyword.contains('sequel') and (mi.info in allowed_infos)) and (t.production_year > 2005)) and (mk.movie_id = mi.movie_id))) then continue;
          _tmp13 := Concat(_tmp13, [t.title]);
        end;
      end;
    end;
  end;
  candidate_titles := _tmp13;
  _tmp14 := specialize TFPGMap<string, integer>.Create;
  _tmp14.AddOrSetData('_tmp14ovie_title', _tmp14in(candidate_titles));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp14]);
  json(_result);
  test_Q3_returns_lexicographically_smallest_sequel_title;
end.
