program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q2_finds_earliest_title_for_German_companies_with_character_keyword;
begin
  if not ((_result = 'Der Film')) then raise Exception.Create('expect failed');
end;

var
  _tmp0: specialize TFPGMap<string, integer>;
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp10: specialize TArray<integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  cn: specialize TFPGMap<string, integer>;
  company_name: specialize TArray<specialize TFPGMap<string, integer>>;
  keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_companies: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_keyword: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: integer;
  title: specialize TArray<specialize TFPGMap<string, integer>>;
  titles: specialize TArray<integer>;

begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('id', 1);
  _tmp0.AddOrSetData('country_code', '[de]');
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('id', 2);
  _tmp1.AddOrSetData('country_code', '[us]');
  company_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp0, _tmp1]);
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('id', 1);
  _tmp2.AddOrSetData('keyword', 'character-na_tmp2e-in-title');
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 2);
  _tmp3.AddOrSetData('keyword', 'other');
  keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2, _tmp3]);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('_tmp4ovie_id', 100);
  _tmp4.AddOrSetData('co_tmp4pany_id', 1);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('_tmp5ovie_id', 200);
  _tmp5.AddOrSetData('co_tmp5pany_id', 2);
  movie_companies := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4, _tmp5]);
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('_tmp6ovie_id', 100);
  _tmp6.AddOrSetData('keyword_id', 1);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('_tmp7ovie_id', 200);
  _tmp7.AddOrSetData('keyword_id', 2);
  movie_keyword := specialize TArray<specialize TFPGMap<string, integer>>([_tmp6, _tmp7]);
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('id', 100);
  _tmp8.AddOrSetData('title', 'Der Fil_tmp8');
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('id', 200);
  _tmp9.AddOrSetData('title', 'Other Movie');
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp8, _tmp9]);
  SetLength(_tmp10, 0);
  for cn in company_name do
    begin
      for mc in movie_companies do
        begin
          if not ((mc.company_id = cn.id)) then continue;
          for t in title do
            begin
              if not ((mc.movie_id = t.id)) then continue;
              for mk in movie_keyword do
                begin
                  if not ((mk.movie_id = t.id)) then continue;
                  for k in keyword do
                    begin
                      if not ((mk.keyword_id = k.id)) then continue;
                      if not ((((cn.country_code = '[de]') and (k.keyword =
                         'character-name-in-title')) and (mc.movie_id = mk.movie_id))) then continue
                      ;
                      _tmp10 := Concat(_tmp10, [t.title]);
                    end;
                end;
            end;
        end;
    end;
  titles := _tmp10;
  _result := min(titles);
  json(_result);
  test_Q2_finds_earliest_title_for_German_companies_with_character_keyword;
end.

