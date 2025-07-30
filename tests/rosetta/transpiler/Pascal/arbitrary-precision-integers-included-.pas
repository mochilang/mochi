{$mode objfpc}
program Main;
uses SysUtils;
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
  e1: integer;
  e2: integer;
  base: int64;
  x: int64;
  s: string;
function pow_int(base: integer; exp: integer): integer; forward;
function pow_big(base: int64; exp: integer): int64; forward;
function pow_int(base: integer; exp: integer): integer;
var
  pow_int_result: integer;
  pow_int_b: integer;
  pow_int_e: integer;
begin
  pow_int_result := 1;
  pow_int_b := base;
  pow_int_e := exp;
  while pow_int_e > 0 do begin
  if (pow_int_e mod 2) = 1 then begin
  pow_int_result := pow_int_result * pow_int_b;
end;
  pow_int_b := pow_int_b * pow_int_b;
  pow_int_e := Trunc(pow_int_e div 2);
end;
  exit(pow_int_result);
end;
function pow_big(base: int64; exp: integer): int64;
var
  pow_big_result: int64;
  pow_big_b: int64;
  pow_big_e: integer;
begin
  pow_big_result := 1;
  pow_big_b := base;
  pow_big_e := exp;
  while pow_big_e > 0 do begin
  if (pow_big_e mod 2) = 1 then begin
  pow_big_result := pow_big_result * pow_big_b;
end;
  pow_big_b := pow_big_b * pow_big_b;
  pow_big_e := Trunc(pow_big_e div 2);
end;
  exit(pow_big_result);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  e1 := pow_int(3, 2);
  e2 := pow_int(4, e1);
  base := 5;
  x := pow_big(base, e2);
  s := IntToStr(x);
  writeln('5^(4^(3^2)) has', ' ', Length(s), ' ', 'digits:', ' ', copy(s, 0+1, (20 - (0))), ' ', '...', ' ', copy(s, Length(s) - 20+1, (Length(s) - (Length(s) - 20))));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
