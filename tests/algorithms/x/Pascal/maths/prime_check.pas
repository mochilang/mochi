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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  number: integer;
function is_prime(number: integer): boolean; forward;
function is_prime(number: integer): boolean;
var
  is_prime_i: integer;
begin
  if number < 0 then begin
  panic('is_prime() only accepts positive integers');
end;
  if (1 < number) and (number < 4) then begin
  exit(true);
end else begin
  if ((number < 2) or ((number mod 2) = 0)) or ((number mod 3) = 0) then begin
  exit(false);
end;
end;
  is_prime_i := 5;
  while (is_prime_i * is_prime_i) <= number do begin
  if ((number mod is_prime_i) = 0) or ((number mod (is_prime_i + 2)) = 0) then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 6;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_prime(0), true)));
  writeln(LowerCase(BoolToStr(is_prime(1), true)));
  writeln(LowerCase(BoolToStr(is_prime(2), true)));
  writeln(LowerCase(BoolToStr(is_prime(3), true)));
  writeln(LowerCase(BoolToStr(is_prime(27), true)));
  writeln(LowerCase(BoolToStr(is_prime(87), true)));
  writeln(LowerCase(BoolToStr(is_prime(563), true)));
  writeln(LowerCase(BoolToStr(is_prime(2999), true)));
  writeln(LowerCase(BoolToStr(is_prime(67483), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
