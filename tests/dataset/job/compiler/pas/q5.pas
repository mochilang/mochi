program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q5_finds_the_lexicographically_first_qualifying_title;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('typical_european__tmp0ovie', 'A Fil_tmp0');
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
  candidate_titles: specialize TArray<integer>;
  company_type: specialize TArray<specialize TFPGMap<string, integer>>;
  ct: specialize TFPGMap<string, integer>;
  info_type: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_companies: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_info: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('ct_id', 1);
  _tmp1.AddOrSetData('kind', 'production co_tmp1panies');
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('ct_id', 2);
  _tmp2.AddOrSetData('kind', 'other');
  company_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('it_id', 10);
  _tmp3.AddOrSetData('info', 'languages');
  info_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3]);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('t_id', 100);
  _tmp4.AddOrSetData('title', 'B Movie');
  _tmp4.AddOrSetData('production_year', 2010);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('t_id', 200);
  _tmp5.AddOrSetData('title', 'A Fil_tmp5');
  _tmp5.AddOrSetData('production_year', 2012);
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('t_id', 300);
  _tmp6.AddOrSetData('title', 'Old Movie');
  _tmp6.AddOrSetData('production_year', 2000);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4, _tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('_tmp7ovie_id', 100);
  _tmp7.AddOrSetData('co_tmp7pany_type_id', 1);
  _tmp7.AddOrSetData('note', 'ACME (France) (theatrical)');
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('_tmp8ovie_id', 200);
  _tmp8.AddOrSetData('co_tmp8pany_type_id', 1);
  _tmp8.AddOrSetData('note', 'ACME (France) (theatrical)');
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('_tmp9ovie_id', 300);
  _tmp9.AddOrSetData('co_tmp9pany_type_id', 1);
  _tmp9.AddOrSetData('note', 'ACME (France) (theatrical)');
  movie_companies := specialize TArray<specialize TFPGMap<string, integer>>([_tmp7, _tmp8, _tmp9]);
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('_tmp10ovie_id', 100);
  _tmp10.AddOrSetData('info', 'Ger_tmp10an');
  _tmp10.AddOrSetData('info_type_id', 10);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('_tmp11ovie_id', 200);
  _tmp11.AddOrSetData('info', 'Swedish');
  _tmp11.AddOrSetData('info_type_id', 10);
  _tmp12 := specialize TFPGMap<string, integer>.Create;
  _tmp12.AddOrSetData('_tmp12ovie_id', 300);
  _tmp12.AddOrSetData('info', 'Ger_tmp12an');
  _tmp12.AddOrSetData('info_type_id', 10);
  movie_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp10, _tmp11, _tmp12]);
  SetLength(_tmp13, 0);
  for ct in company_type do
  begin
    for mc in movie_companies do
    begin
      if not ((mc.company_type_id = ct.ct_id)) then continue;
      for mi in movie_info do
      begin
        if not ((mi.movie_id = mc.movie_id)) then continue;
        for it in info_type do
        begin
          if not ((it.it_id = mi.info_type_id)) then continue;
          for t in title do
          begin
            if not ((t.t_id = mc.movie_id)) then continue;
            if not ((((((ct.kind = 'production companies') and ('(theatrical)' in mc.note)) and (mc.note.IndexOf('(France)') >= 0)) and (t.production_year > 2005)) and (mi.info in specialize TArray<integer>(['Sweden', 'Norway', 'Germany', 'Denmark', 'Swedish', 'Denish', 'Norwegian', 'German'])))) then continue;
            _tmp13 := Concat(_tmp13, [t.title]);
          end;
        end;
      end;
    end;
  end;
  candidate_titles := _tmp13;
  _tmp14 := specialize TFPGMap<string, integer>.Create;
  _tmp14.AddOrSetData('typical_european__tmp14ovie', _tmp14in(candidate_titles));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp14]);
  json(_result);
  test_Q5_finds_the_lexicographically_first_qualifying_title;
end.
