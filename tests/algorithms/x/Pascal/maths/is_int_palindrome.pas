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
  num: integer;
function is_int_palindrome(num: integer): boolean; forward;
procedure main(); forward;
function is_int_palindrome(num: integer): boolean;
var
  is_int_palindrome_n: integer;
  is_int_palindrome_rev: integer;
begin
  if num < 0 then begin
  exit(false);
end;
  is_int_palindrome_n := num;
  is_int_palindrome_rev := 0;
  while is_int_palindrome_n > 0 do begin
  is_int_palindrome_rev := (is_int_palindrome_rev * 10) + (is_int_palindrome_n mod 10);
  is_int_palindrome_n := is_int_palindrome_n div 10;
end;
  exit(is_int_palindrome_rev = num);
end;
procedure main();
begin
  writeln(Ord(is_int_palindrome(-121)));
  writeln(Ord(is_int_palindrome(0)));
  writeln(Ord(is_int_palindrome(10)));
  writeln(Ord(is_int_palindrome(11)));
  writeln(Ord(is_int_palindrome(101)));
  writeln(Ord(is_int_palindrome(120)));
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
