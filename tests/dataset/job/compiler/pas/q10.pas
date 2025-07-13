program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q10_finds_uncredited_voice_actor_in_Russian_movie;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('uncredited_voiced_character', 'Ivan');
  _tmp0.AddOrSetData('russian__tmp0ovie', 'Vodka Drea_tmp0s');
  if not ((_result = specialize TArray<specialize TFPGMap<string, string>>([_tmp0]))) then raise Exception.Create('expect failed');
end;

var
  _tmp1: specialize TFPGMap<string, integer>;
  _tmp10: specialize TFPGMap<string, integer>;
  _tmp11: specialize TFPGMap<string, integer>;
  _tmp12: specialize TFPGMap<string, integer>;
  _tmp13: specialize TFPGMap<string, integer>;
  _tmp14: specialize TFPGMap<string, integer>;
  _tmp15: specialize TFPGMap<string, integer>;
  _tmp16: specialize TArray<specialize TFPGMap<string, integer>>;
  _tmp17: specialize TArray<integer>;
  _tmp18: specialize TArray<integer>;
  _tmp19: specialize TFPGMap<string, integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  cast_info: specialize TArray<specialize TFPGMap<string, integer>>;
  char_name: specialize TArray<specialize TFPGMap<string, integer>>;
  chn: specialize TFPGMap<string, integer>;
  company_name: specialize TArray<specialize TFPGMap<string, integer>>;
  company_type: specialize TArray<specialize TFPGMap<string, integer>>;
  matches: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_companies: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  role_type: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;
  x: specialize TFPGMap<string, integer>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('id', 1);
  _tmp1.AddOrSetData('na_tmp1e', 'Ivan');
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('id', 2);
  _tmp2.AddOrSetData('na_tmp2e', 'Alex');
  char_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('_tmp3ovie_id', 10);
  _tmp3.AddOrSetData('person_role_id', 1);
  _tmp3.AddOrSetData('role_id', 1);
  _tmp3.AddOrSetData('note', 'Soldier (voice) (uncredited)');
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('_tmp4ovie_id', 11);
  _tmp4.AddOrSetData('person_role_id', 2);
  _tmp4.AddOrSetData('role_id', 1);
  _tmp4.AddOrSetData('note', '(voice)');
  cast_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4]);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('id', 1);
  _tmp5.AddOrSetData('country_code', '[ru]');
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('id', 2);
  _tmp6.AddOrSetData('country_code', '[us]');
  company_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 1);
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('id', 2);
  company_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp7, _tmp8]);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('_tmp9ovie_id', 10);
  _tmp9.AddOrSetData('co_tmp9pany_id', 1);
  _tmp9.AddOrSetData('co_tmp9pany_type_id', 1);
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('_tmp10ovie_id', 11);
  _tmp10.AddOrSetData('co_tmp10pany_id', 2);
  _tmp10.AddOrSetData('co_tmp10pany_type_id', 1);
  movie_companies := specialize TArray<specialize TFPGMap<string, integer>>([_tmp9, _tmp10]);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('id', 1);
  _tmp11.AddOrSetData('role', 'actor');
  _tmp12 := specialize TFPGMap<string, integer>.Create;
  _tmp12.AddOrSetData('id', 2);
  _tmp12.AddOrSetData('role', 'director');
  role_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp11, _tmp12]);
  _tmp13 := specialize TFPGMap<string, integer>.Create;
  _tmp13.AddOrSetData('id', 10);
  _tmp13.AddOrSetData('title', 'Vodka Drea_tmp13s');
  _tmp13.AddOrSetData('production_year', 2006);
  _tmp14 := specialize TFPGMap<string, integer>.Create;
  _tmp14.AddOrSetData('id', 11);
  _tmp14.AddOrSetData('title', 'Other Fil_tmp14');
  _tmp14.AddOrSetData('production_year', 2004);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp13, _tmp14]);
  _tmp15 := specialize TFPGMap<string, integer>.Create;
  _tmp15.AddOrSetData('character', chn.na_tmp15e);
  _tmp15.AddOrSetData('_tmp15ovie', t.title);
  SetLength(_tmp16, 0);
  for chn in char_name do
  begin
    for ci in cast_info do
    begin
      if not ((chn.id = ci.person_role_id)) then continue;
      for rt in role_type do
      begin
        if not ((rt.id = ci.role_id)) then continue;
        for t in title do
        begin
          if not ((t.id = ci.movie_id)) then continue;
          for mc in movie_companies do
          begin
            if not ((mc.movie_id = t.id)) then continue;
            for cn in company_name do
            begin
              if not ((cn.id = mc.company_id)) then continue;
              for ct in company_type do
              begin
                if not ((ct.id = mc.company_type_id)) then continue;
                if not (((((ci.note.contains('(voice)') and ci.note.contains('(uncredited)')) and (cn.country_code = '[ru]')) and (rt.role = 'actor')) and (t.production_year > 2005))) then continue;
                _tmp16 := Concat(_tmp16, [_tmp15]);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  matches := _tmp16;
  SetLength(_tmp17, 0);
  for x in matches do
  begin
    _tmp17 := Concat(_tmp17, [x.character]);
  end;
  SetLength(_tmp18, 0);
  for x in matches do
  begin
    _tmp18 := Concat(_tmp18, [x.movie]);
  end;
  _tmp19 := specialize TFPGMap<string, integer>.Create;
  _tmp19.AddOrSetData('uncredited_voiced_character', _tmp19in(_t_tmp19p17));
  _tmp19.AddOrSetData('russian__tmp19ovie', _tmp19in(_t_tmp19p18));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp19]);
  json(_result);
  test_Q10_finds_uncredited_voice_actor_in_Russian_movie;
end.
