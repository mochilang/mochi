program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q6_finds_marvel_movie_with_Robert_Downey;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('_tmp0ovie_keyword', '_tmp0arvel-cine_tmp0atic-universe');
  _tmp0.AddOrSetData('actor_na_tmp0e', 'Downey Robert Jr.');
  _tmp0.AddOrSetData('_tmp0arvel__tmp0ovie', 'Iron Man 3');
  if not ((_result = specialize TArray<specialize TFPGMap<string, string>>([_tmp0]))) then raise Exception.Create('expect failed');
end;

var
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp10: specialize TFPGMap<string, integer>;
  _tmp11: specialize TFPGMap<string, integer>;
  _tmp12: specialize TArray<specialize TFPGMap<string, integer>>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  cast_info: specialize TArray<specialize TFPGMap<string, integer>>;
  ci: specialize TFPGMap<string, integer>;
  keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  name: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('_tmp1ovie_id', 1);
  _tmp1.AddOrSetData('person_id', 101);
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('_tmp2ovie_id', 2);
  _tmp2.AddOrSetData('person_id', 102);
  cast_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 100);
  _tmp3.AddOrSetData('keyword', '_tmp3arvel-cine_tmp3atic-universe');
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('id', 200);
  _tmp4.AddOrSetData('keyword', 'other');
  keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4]);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('_tmp5ovie_id', 1);
  _tmp5.AddOrSetData('keyword_id', 100);
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('_tmp6ovie_id', 2);
  _tmp6.AddOrSetData('keyword_id', 200);
  movie_keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 101);
  _tmp7.AddOrSetData('na_tmp7e', 'Downey Robert Jr.');
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('id', 102);
  _tmp8.AddOrSetData('na_tmp8e', 'Chris Evans');
  name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp7, _tmp8]);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('id', 1);
  _tmp9.AddOrSetData('title', 'Iron Man 3');
  _tmp9.AddOrSetData('production_year', 2013);
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('id', 2);
  _tmp10.AddOrSetData('title', 'Old Movie');
  _tmp10.AddOrSetData('production_year', 2000);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp9, _tmp10]);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('_tmp11ovie_keyword', k.keyword);
  _tmp11.AddOrSetData('actor_na_tmp11e', n.na_tmp11e);
  _tmp11.AddOrSetData('_tmp11arvel__tmp11ovie', t.title);
  SetLength(_tmp12, 0);
  for ci in cast_info do
  begin
    for mk in movie_keyword do
    begin
      if not ((ci.movie_id = mk.movie_id)) then continue;
      for k in keyword do
      begin
        if not ((mk.keyword_id = k.id)) then continue;
        for n in name do
        begin
          if not ((ci.person_id = n.id)) then continue;
          for t in title do
          begin
            if not ((ci.movie_id = t.id)) then continue;
            if not (((((k.keyword = 'marvel-cinematic-universe') and n.name.contains('Downey')) and n.name.contains('Robert')) and (t.production_year > 2010))) then continue;
            _tmp12 := Concat(_tmp12, [_tmp11]);
          end;
        end;
      end;
    end;
  end;
  _result := _tmp12;
  json(_result);
  test_Q6_finds_marvel_movie_with_Robert_Downey;
end.
