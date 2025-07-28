{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  m: array of array of real;
  v: array of real;
  d: real;
  x: array of real;
  i: integer;
  mc: RealArrayArray;
  s: string;
  j: integer;
function det(m: RealArrayArray): real; forward;
function replaceCol(m: RealArrayArray; col: integer; v: RealArray): RealArrayArray; forward;
function det(m: RealArrayArray): real;
var
  det_n: integer;
  det_total: real;
  det_sign: real;
  det_c: integer;
  det_sub: array of RealArray;
  det_r: integer;
  det_row: array of real;
  det_cc: integer;
begin
  det_n := Length(m);
  if det_n = 1 then begin
  exit(m[0][0]);
end;
  det_total := 0;
  det_sign := 1;
  det_c := 0;
  while det_c < det_n do begin
  det_sub := [];
  det_r := 1;
  while det_r < det_n do begin
  det_row := [];
  det_cc := 0;
  while det_cc < det_n do begin
  if det_cc <> det_c then begin
  det_row := concat(det_row, [m[det_r][det_cc]]);
end;
  det_cc := det_cc + 1;
end;
  det_sub := concat(det_sub, [det_row]);
  det_r := det_r + 1;
end;
  det_total := det_total + ((det_sign * m[0][det_c]) * det(det_sub));
  det_sign := det_sign * -1;
  det_c := det_c + 1;
end;
  exit(det_total);
end;
function replaceCol(m: RealArrayArray; col: integer; v: RealArray): RealArrayArray;
var
  replaceCol_res: array of RealArray;
  replaceCol_r: integer;
  replaceCol_row: array of real;
  replaceCol_c: integer;
begin
  replaceCol_res := [];
  replaceCol_r := 0;
  while replaceCol_r < Length(m) do begin
  replaceCol_row := [];
  replaceCol_c := 0;
  while replaceCol_c < Length(m[replaceCol_r]) do begin
  if replaceCol_c = col then begin
  replaceCol_row := concat(replaceCol_row, [v[replaceCol_r]]);
end else begin
  replaceCol_row := concat(replaceCol_row, [m[replaceCol_r][replaceCol_c]]);
end;
  replaceCol_c := replaceCol_c + 1;
end;
  replaceCol_res := concat(replaceCol_res, [replaceCol_row]);
  replaceCol_r := replaceCol_r + 1;
end;
  exit(replaceCol_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  m := [[2, -1, 5, 1], [3, 2, 2, -6], [1, 3, 3, -1], [5, -2, -3, 3]];
  v := [-3, -32, -47, 49];
  d := det(m);
  x := [];
  i := 0;
  while i < Length(v) do begin
  mc := replaceCol(m, i, v);
  x := concat(x, [det(mc) / d]);
  i := i + 1;
end;
  s := '[';
  j := 0;
  while j < Length(x) do begin
  s := s + FloatToStr(x[j]);
  if j < (Length(x) - 1) then begin
  s := s + ' ';
end;
  j := j + 1;
end;
  s := s + ']';
  writeln(s);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
