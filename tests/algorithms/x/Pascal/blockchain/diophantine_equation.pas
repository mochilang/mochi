{$mode objfpc}
program Main;
uses SysUtils, Math;
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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  s1: IntArray;
  sols: IntArrayArray;
  j: integer;
  a: integer;
  n: integer;
  b: integer;
  c: integer;
function gcd(a: integer; b: integer): integer; forward;
function extended_gcd(a: integer; b: integer): IntArray; forward;
function diophantine(a: integer; b: integer; c: integer): IntArray; forward;
function diophantine_all_soln(a: integer; b: integer; c: integer; n: integer): IntArrayArray; forward;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_t: integer;
begin
  gcd_x := IfThen(a < 0, -a, a);
  gcd_y := IfThen(b < 0, -b, b);
  while gcd_y <> 0 do begin
  gcd_t := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_t;
end;
  exit(gcd_x);
end;
function extended_gcd(a: integer; b: integer): IntArray;
var
  extended_gcd_res: array of integer;
  extended_gcd_d: integer;
  extended_gcd_p: integer;
  extended_gcd_q: integer;
  extended_gcd_x: integer;
  extended_gcd_y: integer;
begin
  if b = 0 then begin
  exit([a, 1, 0]);
end;
  extended_gcd_res := extended_gcd(b, a mod b);
  extended_gcd_d := extended_gcd_res[0];
  extended_gcd_p := extended_gcd_res[1];
  extended_gcd_q := extended_gcd_res[2];
  extended_gcd_x := extended_gcd_q;
  extended_gcd_y := extended_gcd_p - (extended_gcd_q * (a div b));
  exit([extended_gcd_d, extended_gcd_x, extended_gcd_y]);
end;
function diophantine(a: integer; b: integer; c: integer): IntArray;
var
  diophantine_d: integer;
  diophantine_eg: IntArray;
  diophantine_r: integer;
  diophantine_x: integer;
  diophantine_y: integer;
begin
  diophantine_d := gcd(a, b);
  if (c mod diophantine_d) <> 0 then begin
  panic('No solution');
end;
  diophantine_eg := extended_gcd(a, b);
  diophantine_r := c div diophantine_d;
  diophantine_x := diophantine_eg[1] * diophantine_r;
  diophantine_y := diophantine_eg[2] * diophantine_r;
  exit([diophantine_x, diophantine_y]);
end;
function diophantine_all_soln(a: integer; b: integer; c: integer; n: integer): IntArrayArray;
var
  diophantine_all_soln_base: IntArray;
  diophantine_all_soln_x0: integer;
  diophantine_all_soln_y0: integer;
  diophantine_all_soln_d: integer;
  diophantine_all_soln_p: integer;
  diophantine_all_soln_q: integer;
  diophantine_all_soln_sols: array of IntArray;
  diophantine_all_soln_i: integer;
  diophantine_all_soln_x: integer;
  diophantine_all_soln_y: integer;
begin
  diophantine_all_soln_base := diophantine(a, b, c);
  diophantine_all_soln_x0 := diophantine_all_soln_base[0];
  diophantine_all_soln_y0 := diophantine_all_soln_base[1];
  diophantine_all_soln_d := gcd(a, b);
  diophantine_all_soln_p := a div diophantine_all_soln_d;
  diophantine_all_soln_q := b div diophantine_all_soln_d;
  diophantine_all_soln_sols := [];
  diophantine_all_soln_i := 0;
  while diophantine_all_soln_i < n do begin
  diophantine_all_soln_x := diophantine_all_soln_x0 + (diophantine_all_soln_i * diophantine_all_soln_q);
  diophantine_all_soln_y := diophantine_all_soln_y0 - (diophantine_all_soln_i * diophantine_all_soln_p);
  diophantine_all_soln_sols := concat(diophantine_all_soln_sols, [[diophantine_all_soln_x, diophantine_all_soln_y]]);
  diophantine_all_soln_i := diophantine_all_soln_i + 1;
end;
  exit(diophantine_all_soln_sols);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  s1 := diophantine(10, 6, 14);
  writeln(list_int_to_str(s1));
  sols := diophantine_all_soln(10, 6, 14, 4);
  j := 0;
  while j < Length(sols) do begin
  writeln(list_int_to_str(sols[j]));
  j := j + 1;
end;
  writeln(list_int_to_str(diophantine(391, 299, -69)));
  writeln(list_int_to_str(extended_gcd(10, 6)));
  writeln(list_int_to_str(extended_gcd(7, 5)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
