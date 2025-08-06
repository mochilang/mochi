{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  b: integer;
  a: integer;
  base: integer;
  n: integer;
  d: integer;
  exp: integer;
  mod_: integer;
  e: integer;
function gcd(a: integer; b: integer): integer; forward;
function pow_mod(base: integer; exp: integer; mod_: integer): integer; forward;
function rsa_factor(d: integer; e: integer; n: integer): IntArray; forward;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_t: integer;
begin
  gcd_x := a;
  gcd_y := b;
  while gcd_y <> 0 do begin
  gcd_t := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_t;
end;
  if gcd_x < 0 then begin
  exit(-gcd_x);
end;
  exit(gcd_x);
end;
function pow_mod(base: integer; exp: integer; mod_: integer): integer;
var
  pow_mod_result_: integer;
  pow_mod_b: integer;
  pow_mod_e: integer;
begin
  pow_mod_result_ := 1;
  pow_mod_b := base mod mod_;
  pow_mod_e := exp;
  while pow_mod_e > 0 do begin
  if (pow_mod_e mod 2) = 1 then begin
  pow_mod_result_ := (pow_mod_result_ * pow_mod_b) mod mod_;
end;
  pow_mod_e := pow_mod_e div 2;
  pow_mod_b := (pow_mod_b * pow_mod_b) mod mod_;
end;
  exit(pow_mod_result_);
end;
function rsa_factor(d: integer; e: integer; n: integer): IntArray;
var
  rsa_factor_k: integer;
  rsa_factor_p: integer;
  rsa_factor_q: integer;
  rsa_factor_g: integer;
  rsa_factor_t: integer;
  rsa_factor_x: integer;
  rsa_factor_y: integer;
begin
  rsa_factor_k := (d * e) - 1;
  rsa_factor_p := 0;
  rsa_factor_q := 0;
  rsa_factor_g := 2;
  while (rsa_factor_p = 0) and (rsa_factor_g < n) do begin
  rsa_factor_t := rsa_factor_k;
  while (rsa_factor_t mod 2) = 0 do begin
  rsa_factor_t := rsa_factor_t div 2;
  rsa_factor_x := pow_mod(rsa_factor_g, rsa_factor_t, n);
  rsa_factor_y := gcd(rsa_factor_x - 1, n);
  if (rsa_factor_x > 1) and (rsa_factor_y > 1) then begin
  rsa_factor_p := rsa_factor_y;
  rsa_factor_q := n div rsa_factor_y;
  break;
end;
end;
  rsa_factor_g := rsa_factor_g + 1;
end;
  if rsa_factor_p > rsa_factor_q then begin
  exit([rsa_factor_q, rsa_factor_p]);
end;
  exit([rsa_factor_p, rsa_factor_q]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(rsa_factor(3, 16971, 25777));
  show_list(rsa_factor(7331, 11, 27233));
  show_list(rsa_factor(4021, 13, 17711));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
