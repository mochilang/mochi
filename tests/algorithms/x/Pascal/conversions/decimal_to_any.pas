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
  base: integer;
function decimal_to_any(num: integer; base: integer): string; forward;
procedure main(); forward;
function decimal_to_any(num: integer; base: integer): string;
var
  decimal_to_any_symbols: string;
  decimal_to_any_n: integer;
  decimal_to_any_result_: string;
  decimal_to_any_mod_: integer;
  decimal_to_any_digit: string;
begin
  if num < 0 then begin
  panic('parameter must be positive int');
end;
  if base < 2 then begin
  panic('base must be >= 2');
end;
  if base > 36 then begin
  panic('base must be <= 36');
end;
  if num = 0 then begin
  exit('0');
end;
  decimal_to_any_symbols := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  decimal_to_any_n := num;
  decimal_to_any_result_ := '';
  while decimal_to_any_n > 0 do begin
  decimal_to_any_mod_ := decimal_to_any_n mod base;
  decimal_to_any_digit := copy(decimal_to_any_symbols, decimal_to_any_mod_+1, (decimal_to_any_mod_ + 1 - (decimal_to_any_mod_)));
  decimal_to_any_result_ := decimal_to_any_digit + decimal_to_any_result_;
  decimal_to_any_n := decimal_to_any_n div base;
end;
  exit(decimal_to_any_result_);
end;
procedure main();
begin
  writeln(decimal_to_any(0, 2));
  writeln(decimal_to_any(5, 4));
  writeln(decimal_to_any(20, 3));
  writeln(decimal_to_any(58, 16));
  writeln(decimal_to_any(243, 17));
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
