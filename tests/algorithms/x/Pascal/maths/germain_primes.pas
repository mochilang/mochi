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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  number: integer;
  n: integer;
function is_prime(n: integer): boolean; forward;
function is_germain_prime(number: integer): boolean; forward;
function is_safe_prime(number: integer): boolean; forward;
procedure test_is_germain_prime(); forward;
procedure test_is_safe_prime(); forward;
procedure main(); forward;
function is_prime(n: integer): boolean;
var
  is_prime_i: integer;
begin
  if n <= 1 then begin
  exit(false);
end;
  if n <= 3 then begin
  exit(true);
end;
  if (n mod 2) = 0 then begin
  exit(false);
end;
  is_prime_i := 3;
  while (is_prime_i * is_prime_i) <= n do begin
  if (n mod is_prime_i) = 0 then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 2;
end;
  exit(true);
end;
function is_germain_prime(number: integer): boolean;
begin
  if number < 1 then begin
  panic('Input value must be a positive integer');
end;
  exit(is_prime(number) and is_prime((2 * number) + 1));
end;
function is_safe_prime(number: integer): boolean;
begin
  if number < 1 then begin
  panic('Input value must be a positive integer');
end;
  if ((number - 1) mod 2) <> 0 then begin
  exit(false);
end;
  exit(is_prime(number) and is_prime((number - 1) div 2));
end;
procedure test_is_germain_prime();
begin
  if not is_germain_prime(3) then begin
  panic('is_germain_prime(3) failed');
end;
  if not is_germain_prime(11) then begin
  panic('is_germain_prime(11) failed');
end;
  if is_germain_prime(4) then begin
  panic('is_germain_prime(4) failed');
end;
  if not is_germain_prime(23) then begin
  panic('is_germain_prime(23) failed');
end;
  if is_germain_prime(13) then begin
  panic('is_germain_prime(13) failed');
end;
  if is_germain_prime(20) then begin
  panic('is_germain_prime(20) failed');
end;
end;
procedure test_is_safe_prime();
begin
  if not is_safe_prime(5) then begin
  panic('is_safe_prime(5) failed');
end;
  if not is_safe_prime(11) then begin
  panic('is_safe_prime(11) failed');
end;
  if is_safe_prime(1) then begin
  panic('is_safe_prime(1) failed');
end;
  if is_safe_prime(2) then begin
  panic('is_safe_prime(2) failed');
end;
  if is_safe_prime(3) then begin
  panic('is_safe_prime(3) failed');
end;
  if not is_safe_prime(47) then begin
  panic('is_safe_prime(47) failed');
end;
end;
procedure main();
begin
  test_is_germain_prime();
  test_is_safe_prime();
  writeln(Ord(is_germain_prime(23)));
  writeln(Ord(is_safe_prime(47)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
