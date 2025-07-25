{$mode objfpc}
program Main;
uses SysUtils, fgl;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
type Anon1 = record
  count: integer;
  list: array of IntArray;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  validComb_square1: integer;
  validComb_square2: integer;
  validComb_square3: integer;
  validComb_square4: integer;
  isUnique_nums: array of integer;
  isUnique_i: integer;
  isUnique_j: integer;
  getCombs_valid: array of array of integer;
  getCombs_count: integer;
  b: integer;
  c: integer;
  d: integer;
  getCombs_s: integer;
  e: integer;
  f: integer;
  getCombs_a: integer;
  getCombs_g: integer;
  r1: Anon1;
  r2: Anon1;
  r3: Anon1;
function makeAnon1(count: integer; list: IntArrayArray): Anon1;
begin
  Result.count := count;
  Result.list := list;
end;
function validComb(a: integer; b: integer; c: integer; d: integer; e: integer; f: integer; g: integer): boolean;
begin
  validComb_square1 := a + b;
  validComb_square2 := (b + c) + d;
  validComb_square3 := (d + e) + f;
  validComb_square4 := f + g;
  exit(((validComb_square1 = validComb_square2) and (validComb_square2 = validComb_square3)) and (validComb_square3 = validComb_square4));
end;
function isUnique(a: integer; b: integer; c: integer; d: integer; e: integer; f: integer; g: integer): boolean;
begin
  isUnique_nums := [a, b, c, d, e, f, g];
  isUnique_i := 0;
  while isUnique_i < Length(isUnique_nums) do begin
  isUnique_j := isUnique_i + 1;
  while isUnique_j < Length(isUnique_nums) do begin
  if isUnique_nums[isUnique_i] = isUnique_nums[isUnique_j] then begin
  exit(false);
end;
  isUnique_j := isUnique_j + 1;
end;
  isUnique_i := isUnique_i + 1;
end;
  exit(true);
end;
function getCombs(low: integer; high: integer; unique: boolean): Anon1;
begin
  getCombs_valid := [];
  getCombs_count := 0;
  for b := low to (high + 1 - 1) do begin
  for c := low to (high + 1 - 1) do begin
  for d := low to (high + 1 - 1) do begin
  getCombs_s := (b + c) + d;
  for e := low to (high + 1 - 1) do begin
  for f := low to (high + 1 - 1) do begin
  getCombs_a := getCombs_s - b;
  getCombs_g := getCombs_s - f;
  if (getCombs_a < low) or (getCombs_a > high) then begin
  continue;
end;
  if (getCombs_g < low) or (getCombs_g > high) then begin
  continue;
end;
  if ((d + e) + f) <> getCombs_s then begin
  continue;
end;
  if (f + getCombs_g) <> getCombs_s then begin
  continue;
end;
  if not unique or isUnique(getCombs_a, b, c, d, e, f, getCombs_g) then begin
  getCombs_valid := concat(getCombs_valid, [[getCombs_a, b, c, d, e, f, getCombs_g]]);
  getCombs_count := getCombs_count + 1;
end;
end;
end;
end;
end;
end;
  exit(makeAnon1(getCombs_count, getCombs_valid));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := getCombs(1, 7, true);
  writeln(IntToStr(r1.count) + ' unique solutions in 1 to 7');
  show_list_list(r1.list);
  r2 := getCombs(3, 9, true);
  writeln(IntToStr(r2.count) + ' unique solutions in 3 to 9');
  show_list_list(r2.list);
  r3 := getCombs(0, 9, false);
  writeln(IntToStr(r3.count) + ' non-unique solutions in 0 to 9');
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
