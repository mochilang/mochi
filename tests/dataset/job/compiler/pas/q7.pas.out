program main;
{$mode objfpc}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

procedure test_Q7_finds_movie_features_biography_for_person;
var
  _tmp0: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('of_person', 'Alan Brown');
  _tmp0.AddOrSetData('biography__tmp0ovie', 'Feature Fil_tmp0');
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
  _tmp21: specialize TFPGMap<string, integer>;
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
  info_type: specialize TArray<specialize TFPGMap<string, integer>>;
  link_type: specialize TArray<specialize TFPGMap<string, integer>>;
  movie_link: specialize TArray<specialize TFPGMap<string, integer>>;
  name: specialize TArray<specialize TFPGMap<string, integer>>;
  person_info: specialize TArray<specialize TFPGMap<string, integer>>;
  r: specialize TFPGMap<string, integer>;
  _result: specialize TArray<specialize TFPGMap<string, integer>>;
  rows: specialize TArray<specialize TFPGMap<string, integer>>;
  title: specialize TArray<specialize TFPGMap<string, integer>>;

begin
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('person_id', 1);
  _tmp1.AddOrSetData('na_tmp1e', 'Anna Mae');
  _tmp2 := specialize TFPGMap<string, integer>.Create;
  _tmp2.AddOrSetData('person_id', 2);
  _tmp2.AddOrSetData('na_tmp2e', 'Chris');
  aka_name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp1, _tmp2]);
  _tmp3 := specialize TFPGMap<string, integer>.Create;
  _tmp3.AddOrSetData('person_id', 1);
  _tmp3.AddOrSetData('_tmp3ovie_id', 10);
  _tmp4 := specialize TFPGMap<string, integer>.Create;
  _tmp4.AddOrSetData('person_id', 2);
  _tmp4.AddOrSetData('_tmp4ovie_id', 20);
  cast_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp3, _tmp4]);
  _tmp5 := specialize TFPGMap<string, integer>.Create;
  _tmp5.AddOrSetData('id', 1);
  _tmp5.AddOrSetData('info', '_tmp5ini biography');
  _tmp6 := specialize TFPGMap<string, integer>.Create;
  _tmp6.AddOrSetData('id', 2);
  _tmp6.AddOrSetData('info', 'trivia');
  info_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp5, _tmp6]);
  _tmp7 := specialize TFPGMap<string, integer>.Create;
  _tmp7.AddOrSetData('id', 1);
  _tmp7.AddOrSetData('link', 'features');
  _tmp8 := specialize TFPGMap<string, integer>.Create;
  _tmp8.AddOrSetData('id', 2);
  _tmp8.AddOrSetData('link', 'references');
  link_type := specialize TArray<specialize TFPGMap<string, integer>>([_tmp7, _tmp8]);
  _tmp9 := specialize TFPGMap<string, integer>.Create;
  _tmp9.AddOrSetData('linked__tmp9ovie_id', 10);
  _tmp9.AddOrSetData('link_type_id', 1);
  _tmp10 := specialize TFPGMap<string, integer>.Create;
  _tmp10.AddOrSetData('linked__tmp10ovie_id', 20);
  _tmp10.AddOrSetData('link_type_id', 2);
  movie_link := specialize TArray<specialize TFPGMap<string, integer>>([_tmp9, _tmp10]);
  _tmp11 := specialize TFPGMap<string, integer>.Create;
  _tmp11.AddOrSetData('id', 1);
  _tmp11.AddOrSetData('na_tmp11e', 'Alan Brown');
  _tmp11.AddOrSetData('na_tmp11e_pcode_cf', 'B');
  _tmp11.AddOrSetData('gender', '_tmp11');
  _tmp12 := specialize TFPGMap<string, integer>.Create;
  _tmp12.AddOrSetData('id', 2);
  _tmp12.AddOrSetData('na_tmp12e', 'Zoe');
  _tmp12.AddOrSetData('na_tmp12e_pcode_cf', 'Z');
  _tmp12.AddOrSetData('gender', 'f');
  name := specialize TArray<specialize TFPGMap<string, integer>>([_tmp11, _tmp12]);
  _tmp13 := specialize TFPGMap<string, integer>.Create;
  _tmp13.AddOrSetData('person_id', 1);
  _tmp13.AddOrSetData('info_type_id', 1);
  _tmp13.AddOrSetData('note', 'Volker Boeh_tmp13');
  _tmp14 := specialize TFPGMap<string, integer>.Create;
  _tmp14.AddOrSetData('person_id', 2);
  _tmp14.AddOrSetData('info_type_id', 1);
  _tmp14.AddOrSetData('note', 'Other');
  person_info := specialize TArray<specialize TFPGMap<string, integer>>([_tmp13, _tmp14]);
  _tmp15 := specialize TFPGMap<string, integer>.Create;
  _tmp15.AddOrSetData('id', 10);
  _tmp15.AddOrSetData('title', 'Feature Fil_tmp15');
  _tmp15.AddOrSetData('production_year', 1990);
  _tmp16 := specialize TFPGMap<string, integer>.Create;
  _tmp16.AddOrSetData('id', 20);
  _tmp16.AddOrSetData('title', 'Late Fil_tmp16');
  _tmp16.AddOrSetData('production_year', 2000);
  title := specialize TArray<specialize TFPGMap<string, integer>>([_tmp15, _tmp16]);
  _tmp17 := specialize TFPGMap<string, integer>.Create;
  _tmp17.AddOrSetData('person_na_tmp17e', n.na_tmp17e);
  _tmp17.AddOrSetData('_tmp17ovie_title', t.title);
  SetLength(_tmp18, 0);
  for an in aka_name do
  begin
    for n in name do
    begin
      if not ((n.id = an.person_id)) then continue;
      for pi in person_info do
      begin
        if not ((pi.person_id = an.person_id)) then continue;
        for it in info_type do
        begin
          if not ((it.id = pi.info_type_id)) then continue;
          for ci in cast_info do
          begin
            if not ((ci.person_id = n.id)) then continue;
            for t in title do
            begin
              if not ((t.id = ci.movie_id)) then continue;
              for ml in movie_link do
              begin
                if not ((ml.linked_movie_id = t.id)) then continue;
                for lt in link_type do
                begin
                  if not ((lt.id = ml.link_type_id)) then continue;
                  if not (((((((((((((an.name.contains('a') and (it.info = 'mini biography')) and (lt.link = 'features')) and (n.name_pcode_cf >= 'A')) and (n.name_pcode_cf <= 'F')) and ((n.gender = 'm') or ((n.gender = 'f') and n.name.starts_with('B')))) and (pi.note = 'Volker Boehm')) and (t.production_year >= 1980)) and (t.production_year <= 1995)) and (pi.person_id = an.person_id)) and (pi.person_id = ci.person_id)) and (an.person_id = ci.person_id)) and (ci.movie_id = ml.linked_movie_id))) then continue;
                  _tmp18 := Concat(_tmp18, [_tmp17]);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  rows := _tmp18;
  SetLength(_tmp19, 0);
  for r in rows do
  begin
    _tmp19 := Concat(_tmp19, [r.person_name]);
  end;
  SetLength(_tmp20, 0);
  for r in rows do
  begin
    _tmp20 := Concat(_tmp20, [r.movie_title]);
  end;
  _tmp21 := specialize TFPGMap<string, integer>.Create;
  _tmp21.AddOrSetData('of_person', _tmp21in(_t_tmp21p19));
  _tmp21.AddOrSetData('biography__tmp21ovie', _tmp21in(_t_tmp21p20));
  _result := specialize TArray<specialize TFPGMap<string, integer>>([_tmp21]);
  json(_result);
  test_Q7_finds_movie_features_biography_for_person;
end.
