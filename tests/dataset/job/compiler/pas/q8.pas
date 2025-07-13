program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('actress_pseudony_tmp0', 'Y. S.');
  _tmp0.AddOrSetData('japanese__tmp0ovie_dubbed', 'Dubbed Fil_tmp0');
  if not ((_result = specialize TArray<specialize TFPGMap<string, string>>([_tmp0]))) then raise Exception.Create('expect failed');
end;

var
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp10: specialize TArray<specialize TFPGMap<string, integer>>;
  _tmp11: specialize TArray<integer>;
  _tmp12: specialize TArray<integer>;
  _tmp13: specialize TFPGMap<string, integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  aka_name: specialize TArray<specialize TFPGMap<string, integer>>;
  an1: specialize TFPGMap<string, integer>;
  cast_info: specialize TArray<specialize TFPGMap<string, integer>>;
  company_name: specialize TArray<specialize TFPGMap<string, integer>>;
  eligible: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_companies: specialize TArray<specialize TFPGMap<string, integer>>;
  name: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  role_type: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;
  x: specialize TFPGMap<string, integer>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('person_id', 1);
  _tmp1.AddOrSetData('na_tmp1e', 'Y. S.');
  aka_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1]);
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('person_id', 1);
  _tmp2.AddOrSetData('_tmp2ovie_id', 10);
  _tmp2.AddOrSetData('note', '(voice: English version)');
  _tmp2.AddOrSetData('role_id', 1000);
  cast_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 50);
  _tmp3.AddOrSetData('country_code', '[jp]');
  company_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3]);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('_tmp4ovie_id', 10);
  _tmp4.AddOrSetData('co_tmp4pany_id', 50);
  _tmp4.AddOrSetData('note', 'Studio (Japan)');
  movie_companies := specialize TArray<specialize TFPGMap<string, integer>>([_tmp4]);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('id', 1);
  _tmp5.AddOrSetData('na_tmp5e', 'Yoko Ono');
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('id', 2);
  _tmp6.AddOrSetData('na_tmp6e', 'Yuichi');
  name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 1000);
  _tmp7.AddOrSetData('role', 'actress');
  role_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp7]);
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('id', 10);
  _tmp8.AddOrSetData('title', 'Dubbed Fil_tmp8');
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp8]);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('pseudony_tmp9', an1.na_tmp9e);
  _tmp9.AddOrSetData('_tmp9ovie_title', t.title);
  SetLength(_tmp10, 0);
  for an1 in aka_name do
  begin
    for n1 in name do
    begin
      if not ((n1.id = an1.person_id)) then continue;
      for ci in cast_info do
      begin
        if not ((ci.person_id = an1.person_id)) then continue;
        for t in title do
        begin
          if not ((t.id = ci.movie_id)) then continue;
          for mc in movie_companies do
          begin
            if not ((mc.movie_id = ci.movie_id)) then continue;
            for cn in company_name do
            begin
              if not ((cn.id = mc.company_id)) then continue;
              for rt in role_type do
              begin
                if not ((rt.id = ci.role_id)) then continue;
                if not ((((((((ci.note = '(voice: English version)') and (cn.country_code = '[jp]')) and mc.note.contains('(Japan)')) and not mc.note.contains('(USA)')) and n1.name.contains('Yo')) and not n1.name.contains('Yu')) and (rt.role = 'actress'))) then continue;
                _tmp10 := Concat(_tmp10, [_tmp9]);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  eligible := _tmp10;
  SetLength(_tmp11, 0);
  for x in eligible do
  begin
    _tmp11 := Concat(_tmp11, [x.pseudonym]);
  end;
  SetLength(_tmp12, 0);
  for x in eligible do
  begin
    _tmp12 := Concat(_tmp12, [x.movie_title]);
  end;
  _tmp13 := specialize TFPGMap<string, integer>.Create;
  _tmp13.AddOrSetData('actress_pseudony_tmp13', _tmp13in(_t_tmp13p11));
  _tmp13.AddOrSetData('japanese__tmp13ovie_dubbed', _tmp13in(_t_tmp13p12));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp13]);
  json(_result);
  test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing;
end.
