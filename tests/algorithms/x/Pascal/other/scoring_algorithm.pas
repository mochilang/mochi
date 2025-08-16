{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type RealArrayArray = array of RealArray;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_real_to_str(xs: array of RealArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_real_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  vehicles: array of RealArray;
  weights: array of integer;
  result_: RealArrayArray;
  source_data: RealArrayArray;
  data_lists: RealArrayArray;
  score_lists: RealArrayArray;
function get_data(source_data: RealArrayArray): RealArrayArray; forward;
function calculate_each_score(data_lists: RealArrayArray; weights: IntArray): RealArrayArray; forward;
function generate_final_scores(score_lists: RealArrayArray): RealArray; forward;
function procentual_proximity(source_data: RealArrayArray; weights: IntArray): RealArrayArray; forward;
function get_data(source_data: RealArrayArray): RealArrayArray;
var
  get_data_data_lists: array of RealArray;
  get_data_i: integer;
  get_data_row: array of real;
  get_data_j: integer;
  get_data_empty: array of real;
begin
  get_data_data_lists := [];
  get_data_i := 0;
  while get_data_i < Length(source_data) do begin
  get_data_row := source_data[get_data_i];
  get_data_j := 0;
  while get_data_j < Length(get_data_row) do begin
  if Length(get_data_data_lists) < (get_data_j + 1) then begin
  get_data_empty := [];
  get_data_data_lists := concat(get_data_data_lists, [get_data_empty]);
end;
  get_data_data_lists[get_data_j] := concat(get_data_data_lists[get_data_j], [get_data_row[get_data_j]]);
  get_data_j := get_data_j + 1;
end;
  get_data_i := get_data_i + 1;
end;
  exit(get_data_data_lists);
end;
function calculate_each_score(data_lists: RealArrayArray; weights: IntArray): RealArrayArray;
var
  calculate_each_score_score_lists: array of RealArray;
  calculate_each_score_i: integer;
  calculate_each_score_dlist: array of real;
  calculate_each_score_weight: integer;
  calculate_each_score_mind: real;
  calculate_each_score_maxd: real;
  calculate_each_score_j: integer;
  calculate_each_score_val: real;
  calculate_each_score_score: array of real;
  calculate_each_score_item: real;
begin
  calculate_each_score_score_lists := [];
  calculate_each_score_i := 0;
  while calculate_each_score_i < Length(data_lists) do begin
  calculate_each_score_dlist := data_lists[calculate_each_score_i];
  calculate_each_score_weight := weights[calculate_each_score_i];
  calculate_each_score_mind := calculate_each_score_dlist[0];
  calculate_each_score_maxd := calculate_each_score_dlist[0];
  calculate_each_score_j := 1;
  while calculate_each_score_j < Length(calculate_each_score_dlist) do begin
  calculate_each_score_val := calculate_each_score_dlist[calculate_each_score_j];
  if calculate_each_score_val < calculate_each_score_mind then begin
  calculate_each_score_mind := calculate_each_score_val;
end;
  if calculate_each_score_val > calculate_each_score_maxd then begin
  calculate_each_score_maxd := calculate_each_score_val;
end;
  calculate_each_score_j := calculate_each_score_j + 1;
end;
  calculate_each_score_score := [];
  calculate_each_score_j := 0;
  if calculate_each_score_weight = 0 then begin
  while calculate_each_score_j < Length(calculate_each_score_dlist) do begin
  calculate_each_score_item := calculate_each_score_dlist[calculate_each_score_j];
  if (calculate_each_score_maxd - calculate_each_score_mind) = 0 then begin
  calculate_each_score_score := concat(calculate_each_score_score, [1]);
end else begin
  calculate_each_score_score := concat(calculate_each_score_score, [1 - ((calculate_each_score_item - calculate_each_score_mind) / (calculate_each_score_maxd - calculate_each_score_mind))]);
end;
  calculate_each_score_j := calculate_each_score_j + 1;
end;
end else begin
  while calculate_each_score_j < Length(calculate_each_score_dlist) do begin
  calculate_each_score_item := calculate_each_score_dlist[calculate_each_score_j];
  if (calculate_each_score_maxd - calculate_each_score_mind) = 0 then begin
  calculate_each_score_score := concat(calculate_each_score_score, [0]);
end else begin
  calculate_each_score_score := concat(calculate_each_score_score, [(calculate_each_score_item - calculate_each_score_mind) / (calculate_each_score_maxd - calculate_each_score_mind)]);
end;
  calculate_each_score_j := calculate_each_score_j + 1;
end;
end;
  calculate_each_score_score_lists := concat(calculate_each_score_score_lists, [calculate_each_score_score]);
  calculate_each_score_i := calculate_each_score_i + 1;
end;
  exit(calculate_each_score_score_lists);
end;
function generate_final_scores(score_lists: RealArrayArray): RealArray;
var
  generate_final_scores_count: integer;
  generate_final_scores_final_scores: array of real;
  generate_final_scores_i: integer;
  generate_final_scores_slist: array of real;
  generate_final_scores_j: integer;
begin
  generate_final_scores_count := Length(score_lists[0]);
  generate_final_scores_final_scores := [];
  generate_final_scores_i := 0;
  while generate_final_scores_i < generate_final_scores_count do begin
  generate_final_scores_final_scores := concat(generate_final_scores_final_scores, [0]);
  generate_final_scores_i := generate_final_scores_i + 1;
end;
  generate_final_scores_i := 0;
  while generate_final_scores_i < Length(score_lists) do begin
  generate_final_scores_slist := score_lists[generate_final_scores_i];
  generate_final_scores_j := 0;
  while generate_final_scores_j < Length(generate_final_scores_slist) do begin
  generate_final_scores_final_scores[generate_final_scores_j] := generate_final_scores_final_scores[generate_final_scores_j] + generate_final_scores_slist[generate_final_scores_j];
  generate_final_scores_j := generate_final_scores_j + 1;
end;
  generate_final_scores_i := generate_final_scores_i + 1;
end;
  exit(generate_final_scores_final_scores);
end;
function procentual_proximity(source_data: RealArrayArray; weights: IntArray): RealArrayArray;
var
  procentual_proximity_data_lists: RealArrayArray;
  procentual_proximity_score_lists: RealArrayArray;
  procentual_proximity_final_scores: RealArray;
  procentual_proximity_i: integer;
begin
  procentual_proximity_data_lists := get_data(source_data);
  procentual_proximity_score_lists := calculate_each_score(procentual_proximity_data_lists, weights);
  procentual_proximity_final_scores := generate_final_scores(procentual_proximity_score_lists);
  procentual_proximity_i := 0;
  while procentual_proximity_i < Length(procentual_proximity_final_scores) do begin
  source_data[procentual_proximity_i] := concat(source_data[procentual_proximity_i], [procentual_proximity_final_scores[procentual_proximity_i]]);
  procentual_proximity_i := procentual_proximity_i + 1;
end;
  exit(source_data);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  vehicles := [];
  vehicles := concat(vehicles, [[20, 60, 2012]]);
  vehicles := concat(vehicles, [[23, 90, 2015]]);
  vehicles := concat(vehicles, [[22, 50, 2011]]);
  weights := [0, 0, 1];
  result_ := procentual_proximity(vehicles, weights);
  writeln(list_list_real_to_str(result_));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
