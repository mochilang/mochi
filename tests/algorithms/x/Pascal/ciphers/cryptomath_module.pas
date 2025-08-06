{$mode objfpc}
program Main;
uses SysUtils, Math;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  b: integer;
  m: integer;
  a: integer;
function gcd(a: integer; b: integer): integer; forward;
function find_mod_inverse(a: integer; m: integer): integer; forward;
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
function find_mod_inverse(a: integer; m: integer): integer;
var
  find_mod_inverse_u1: integer;
  find_mod_inverse_u2: integer;
  find_mod_inverse_u3: integer;
  find_mod_inverse_v1: integer;
  find_mod_inverse_v2: integer;
  find_mod_inverse_v3: integer;
  find_mod_inverse_q: integer;
  find_mod_inverse_t1: integer;
  find_mod_inverse_t2: integer;
  find_mod_inverse_t3: integer;
  find_mod_inverse_res: integer;
begin
  if gcd(a, m) <> 1 then begin
  error(((('mod inverse of ' + IntToStr(a)) + ' and ') + IntToStr(m)) + ' does not exist');
end;
  find_mod_inverse_u1 := 1;
  find_mod_inverse_u2 := 0;
  find_mod_inverse_u3 := a;
  find_mod_inverse_v1 := 0;
  find_mod_inverse_v2 := 1;
  find_mod_inverse_v3 := m;
  while find_mod_inverse_v3 <> 0 do begin
  find_mod_inverse_q := find_mod_inverse_u3 div find_mod_inverse_v3;
  find_mod_inverse_t1 := find_mod_inverse_u1 - (find_mod_inverse_q * find_mod_inverse_v1);
  find_mod_inverse_t2 := find_mod_inverse_u2 - (find_mod_inverse_q * find_mod_inverse_v2);
  find_mod_inverse_t3 := find_mod_inverse_u3 - (find_mod_inverse_q * find_mod_inverse_v3);
  find_mod_inverse_u1 := find_mod_inverse_v1;
  find_mod_inverse_u2 := find_mod_inverse_v2;
  find_mod_inverse_u3 := find_mod_inverse_v3;
  find_mod_inverse_v1 := find_mod_inverse_t1;
  find_mod_inverse_v2 := find_mod_inverse_t2;
  find_mod_inverse_v3 := find_mod_inverse_t3;
end;
  find_mod_inverse_res := find_mod_inverse_u1 mod m;
  if find_mod_inverse_res < 0 then begin
  find_mod_inverse_res := find_mod_inverse_res + m;
end;
  exit(find_mod_inverse_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(find_mod_inverse(3, 11)));
  writeln(IntToStr(find_mod_inverse(7, 26)));
  writeln(IntToStr(find_mod_inverse(11, 26)));
  writeln(IntToStr(find_mod_inverse(17, 43)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
