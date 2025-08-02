{$mode objfpc}
program Main;
uses SysUtils, fgl;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type SpecializeTFPGMapStringIntegerArray = array of specialize TFPGMap<string, integer>;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  it: integer;
  dir: array of integer;
  nv: integer;
  s: string;
function Map3(h: SpecializeTFPGMapStringIntegerArray; nv: integer; step_best: IntArray): specialize TFPGMap<string, Variant>; forward;
function Map2(it: specialize TFPGMap<string, integer>): specialize TFPGMap<string, integer>; forward;
function Map1(nv: integer): specialize TFPGMap<string, integer>; forward;
function push(h: SpecializeTFPGMapStringIntegerArray; it: specialize TFPGMap<string, integer>): SpecializeTFPGMapStringIntegerArray; forward;
function step(h: SpecializeTFPGMapStringIntegerArray; nv: integer; dir: IntArray): specialize TFPGMap<string, Variant>; forward;
function positions(n: integer): IntArrayArray; forward;
function pad(s: string; w: integer): string; forward;
procedure main(); forward;
function Map3(h: SpecializeTFPGMapStringIntegerArray; nv: integer; step_best: IntArray): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('d', Variant(step_best));
  Result.AddOrSetData('heap', Variant(h));
  Result.AddOrSetData('n', Variant(nv));
end;
function Map2(it: specialize TFPGMap<string, integer>): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('s', Variant((it['a'] * it['a']) + ((it['b'] + 1) * (it['b'] + 1))));
  Result.AddOrSetData('a', Variant(it['a']));
  Result.AddOrSetData('b', Variant(it['b'] + 1));
end;
function Map1(nv: integer): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('s', Variant(nv * nv));
  Result.AddOrSetData('a', Variant(nv));
  Result.AddOrSetData('b', Variant(0));
end;
function push(h: SpecializeTFPGMapStringIntegerArray; it: specialize TFPGMap<string, integer>): SpecializeTFPGMapStringIntegerArray;
var
  push_i: integer;
  push_tmp: integer;
begin
  h := concat(h, [it]);
  push_i := Length(h) - 1;
  while (push_i > 0) and (h[push_i - 1]['s'] > h[push_i]['s']) do begin
  push_tmp := h[push_i - 1];
  h[push_i - 1] := h[push_i];
  h[push_i] := push_tmp;
  push_i := push_i - 1;
end;
  exit(h);
end;
function step(h: SpecializeTFPGMapStringIntegerArray; nv: integer; dir: IntArray): specialize TFPGMap<string, Variant>;
var
  step_s: integer;
  step_v: array of IntArray;
  step_list: array of IntArray;
  step_p: IntArray;
  step_temp: array of IntArray;
  step_bestDot: integer;
  step_best: array of integer;
  step_cross: integer;
  step_dot: integer;
begin
  while (Length(h) = 0) or ((nv * nv) <= h[0]['s']) do begin
  h := push(h, Map1(nv));
  nv := nv + 1;
end;
  step_s := h[0]['s'];
  step_v := [];
  while (Length(h) > 0) and (h[0]['s'] = step_s) do begin
  it := h[0];
  h := copy(h, 1, Length(h));
  step_v := concat(step_v, [[it['a'], it['b']]]);
  if it['a'] > it['b'] then begin
  h := push(h, Map2(it));
end;
end;
  step_list := [];
  for step_p in step_v do begin
  step_list := concat(step_list, [step_p]);
end;
  step_temp := step_list;
  for step_p in step_temp do begin
  if step_p[0] <> step_p[1] then begin
  step_list := concat(step_list, [[step_p[1], step_p[0]]]);
end;
end;
  step_temp := step_list;
  for step_p in step_temp do begin
  if step_p[1] <> 0 then begin
  step_list := concat(step_list, [[step_p[0], -step_p[1]]]);
end;
end;
  step_temp := step_list;
  for step_p in step_temp do begin
  if step_p[0] <> 0 then begin
  step_list := concat(step_list, [[-step_p[0], step_p[1]]]);
end;
end;
  step_bestDot := -999999999;
  step_best := dir;
  for step_p in step_list do begin
  step_cross := (step_p[0] * dir[1]) - (step_p[1] * dir[0]);
  if step_cross >= 0 then begin
  step_dot := (step_p[0] * dir[0]) + (step_p[1] * dir[1]);
  if step_dot > step_bestDot then begin
  step_bestDot := step_dot;
  step_best := step_p;
end;
end;
end;
  exit(Map3(h, nv, step_best));
end;
function positions(n: integer): IntArrayArray;
var
  positions_pos: array of IntArray;
  positions_x: integer;
  positions_y: integer;
  positions_heap: array of specialize TFPGMap<string, integer>;
  positions_i: integer;
  positions_st: specialize TFPGMap<string, Variant>;
begin
  positions_pos := [];
  positions_x := 0;
  positions_y := 0;
  dir := [0, 1];
  positions_heap := [];
  nv := 1;
  positions_i := 0;
  while positions_i < n do begin
  positions_pos := concat(positions_pos, [[positions_x, positions_y]]);
  positions_st := step(positions_heap, nv, dir);
  dir := positions_st['d'];
  positions_heap := positions_st['heap'];
  nv := Trunc(positions_st['n']);
  positions_x := positions_x + dir[0];
  positions_y := positions_y + dir[1];
  positions_i := positions_i + 1;
end;
  exit(positions_pos);
end;
function pad(s: string; w: integer): string;
var
  pad_r: string;
begin
  pad_r := s;
  while Length(pad_r) < w do begin
  pad_r := pad_r + ' ';
end;
  exit(pad_r);
end;
procedure main();
var
  main_pts: IntArrayArray;
  main_line: string;
  main_i: integer;
  main_p: IntArray;
begin
  main_pts := positions(40);
  writeln('The first 40 Babylonian spiral points are:');
  main_line := '';
  main_i := 0;
  while main_i < Length(main_pts) do begin
  main_p := main_pts[main_i];
  s := pad(((('(' + IntToStr(main_p[0])) + ', ') + IntToStr(main_p[1])) + ')', 10);
  main_line := main_line + s;
  if ((main_i + 1) mod 10) = 0 then begin
  writeln(main_line);
  main_line := '';
end;
  main_i := main_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
