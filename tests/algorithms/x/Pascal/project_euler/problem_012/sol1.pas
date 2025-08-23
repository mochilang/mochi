{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function count_divisors(count_divisors_n: int64): int64; forward;
function solution(): int64; forward;
function count_divisors(count_divisors_n: int64): int64;
var
  count_divisors_m: int64;
  count_divisors_n_divisors: int64;
  count_divisors_i: int64;
  count_divisors_multiplicity: int64;
begin
  count_divisors_m := count_divisors_n;
  count_divisors_n_divisors := 1;
  count_divisors_i := 2;
  while (count_divisors_i * count_divisors_i) <= count_divisors_m do begin
  count_divisors_multiplicity := 0;
  while (count_divisors_m mod count_divisors_i) = 0 do begin
  count_divisors_m := count_divisors_m div count_divisors_i;
  count_divisors_multiplicity := count_divisors_multiplicity + 1;
end;
  count_divisors_n_divisors := count_divisors_n_divisors * (count_divisors_multiplicity + 1);
  count_divisors_i := count_divisors_i + 1;
end;
  if count_divisors_m > 1 then begin
  count_divisors_n_divisors := count_divisors_n_divisors * 2;
end;
  exit(count_divisors_n_divisors);
end;
function solution(): int64;
var
  solution_t_num: int64;
  solution_i: int64;
begin
  solution_t_num := 1;
  solution_i := 1;
  while true do begin
  solution_i := solution_i + 1;
  solution_t_num := solution_t_num + solution_i;
  if count_divisors(solution_t_num) > 500 then begin
  break;
end;
end;
  exit(solution_t_num);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution()));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
