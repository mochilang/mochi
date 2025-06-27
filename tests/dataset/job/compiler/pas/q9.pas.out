program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q9_selects_minimal_alternative_name__character_and_movie;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('alternative_na_tmp0e', 'A. N. G.');
  _tmp0.AddOrSetData('character_na_tmp0e', 'Angel');
  _tmp0.AddOrSetData('_tmp0ovie', 'Fa_tmp0ous Fil_tmp0');
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
  _tmp16: specialize TFPGMap<string, integer>;
  _tmp17: specialize TFPGMap<string, integer>;
  _tmp18: specialize TArray<specialize TFPGMap<string, integer>>;
  _tmp19: specialize TArray<integer>;
  _tmp2: specialize TFPGMap<string, integer>;
  _tmp20: specialize TArray<integer>;
  _tmp21: specialize TArray<integer>;
  _tmp22: specialize TFPGMap<string, integer>;
  _tmp3: specialize TFPGMap<string, integer>;
  _tmp4: specialize TFPGMap<string, integer>;
  _tmp5: specialize TFPGMap<string, integer>;
  _tmp6: specialize TFPGMap<string, integer>;
  _tmp7: specialize TFPGMap<string, integer>;
  _tmp8: specialize TFPGMap<string, integer>;
  _tmp9: specialize TFPGMap<string, integer>;
  aka_name: specialize TArray<specialize TFPGMap<string, integer>>;
  an: specialize TFPGMap<string, integer>;
  cast_info: specialize TArray<specialize TFPGMap<string, integer>>;
  char_name: specialize TArray<specialize TFPGMap<string, integer>>;
  company_name: specialize TArray<specialize TFPGMap<string, integer>>;
  matches: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_companies: specialize TArray<specialize TFPGMap<string, integer>>;
  name: specialize TArray<specialize TFPGMap<string, integer>>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  role_type: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;
  x: specialize TFPGMap<string, integer>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('person_id', 1);
  _tmp1.AddOrSetData('na_tmp1e', 'A. N. G.');
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('person_id', 2);
  _tmp2.AddOrSetData('na_tmp2e', 'J. D.');
  aka_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('id', 10);
  _tmp3.AddOrSetData('na_tmp3e', 'Angel');
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('id', 20);
  _tmp4.AddOrSetData('na_tmp4e', 'Devil');
  char_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4]);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('person_id', 1);
  _tmp5.AddOrSetData('person_role_id', 10);
  _tmp5.AddOrSetData('_tmp5ovie_id', 100);
  _tmp5.AddOrSetData('role_id', 1000);
  _tmp5.AddOrSetData('note', '(voice)');
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('person_id', 2);
  _tmp6.AddOrSetData('person_role_id', 20);
  _tmp6.AddOrSetData('_tmp6ovie_id', 200);
  _tmp6.AddOrSetData('role_id', 1000);
  _tmp6.AddOrSetData('note', '(voice)');
  cast_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 100);
  _tmp7.AddOrSetData('country_code', '[us]');
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('id', 200);
  _tmp8.AddOrSetData('country_code', '[gb]');
  company_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp7, _tmp8]);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('_tmp9ovie_id', 100);
  _tmp9.AddOrSetData('co_tmp9pany_id', 100);
  _tmp9.AddOrSetData('note', 'ACME Studios (USA)');
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('_tmp10ovie_id', 200);
  _tmp10.AddOrSetData('co_tmp10pany_id', 200);
  _tmp10.AddOrSetData('note', 'Maple Fil_tmp10s');
  movie_companies := specialize TArray<specialize TFPGMap<string, integer>>([_tmp9, _tmp10]);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('id', 1);
  _tmp11.AddOrSetData('na_tmp11e', 'Angela S_tmp11ith');
  _tmp11.AddOrSetData('gender', 'f');
  _tmp12 := specialize TFPGMap<string, integer>.Create;
  _tmp12.AddOrSetData('id', 2);
  _tmp12.AddOrSetData('na_tmp12e', 'John Doe');
  _tmp12.AddOrSetData('gender', '_tmp12');
  name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp11, _tmp12]);
  _tmp13 := specialize TFPGMap<string, integer>.Create;
  _tmp13.AddOrSetData('id', 1000);
  _tmp13.AddOrSetData('role', 'actress');
  _tmp14 := specialize TFPGMap<string, integer>.Create;
  _tmp14.AddOrSetData('id', 2000);
  _tmp14.AddOrSetData('role', 'actor');
  role_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp13, _tmp14]);
  _tmp15 := specialize TFPGMap<string, integer>.Create;
  _tmp15.AddOrSetData('id', 100);
  _tmp15.AddOrSetData('title', 'Fa_tmp15ous Fil_tmp15');
  _tmp15.AddOrSetData('production_year', 2010);
  _tmp16 := specialize TFPGMap<string, integer>.Create;
  _tmp16.AddOrSetData('id', 200);
  _tmp16.AddOrSetData('title', 'Old Movie');
  _tmp16.AddOrSetData('production_year', 1999);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp15, _tmp16]);
  _tmp17 := specialize TFPGMap<string, integer>.Create;
  _tmp17.AddOrSetData('alt', an.na_tmp17e);
  _tmp17.AddOrSetData('character', chn.na_tmp17e);
  _tmp17.AddOrSetData('_tmp17ovie', t.title);
  SetLength(_tmp18, 0);
  for an in aka_name do
  begin
    for n in name do
    begin
      if not ((an.person_id = n.id)) then continue;
      for ci in cast_info do
      begin
        if not ((ci.person_id = n.id)) then continue;
        for chn in char_name do
        begin
          if not ((chn.id = ci.person_role_id)) then continue;
          for t in title do
          begin
            if not ((t.id = ci.movie_id)) then continue;
            for mc in movie_companies do
            begin
              if not ((mc.movie_id = t.id)) then continue;
              for cn in company_name do
              begin
                if not ((cn.id = mc.company_id)) then continue;
                for rt in role_type do
                begin
                  if not ((rt.id = ci.role_id)) then continue;
                  if not (((((((((ci.note in specialize TArray<specialize TFPGMap<string, integer>>(['(voice)', '(voice: Japanese version)', '(voice) (uncredited)', '(voice: English version)'])) and (cn.country_code = '[us]')) and (mc.note.contains('(USA)') or mc.note.contains('(worldwide)'))) and (n.gender = 'f')) and n.name.contains('Ang')) and (rt.role = 'actress')) and (t.production_year >= 2005)) and (t.production_year <= 2015))) then continue;
                  _tmp18 := Concat(_tmp18, [_tmp17]);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  matches := _tmp18;
  SetLength(_tmp19, 0);
  for x in matches do
  begin
    _tmp19 := Concat(_tmp19, [x.alt]);
  end;
  SetLength(_tmp20, 0);
  for x in matches do
  begin
    _tmp20 := Concat(_tmp20, [x.character]);
  end;
  SetLength(_tmp21, 0);
  for x in matches do
  begin
    _tmp21 := Concat(_tmp21, [x.movie]);
  end;
  _tmp22 := specialize TFPGMap<string, integer>.Create;
  _tmp22.AddOrSetData('alternative_na_tmp22e', _tmp22in(_t_tmp22p19));
  _tmp22.AddOrSetData('character_na_tmp22e', _tmp22in(_t_tmp22p20));
  _tmp22.AddOrSetData('_tmp22ovie', _tmp22in(_t_tmp22p21));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp22]);
  json(_result);
  test_Q9_selects_minimal_alternative_name__character_and_movie;
end.
