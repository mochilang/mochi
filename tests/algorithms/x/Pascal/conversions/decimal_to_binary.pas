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
  n: integer;
function decimal_to_binary_iterative(num: integer): string; forward;
function decimal_to_binary_recursive_helper(n: integer): string; forward;
function decimal_to_binary_recursive(num: integer): string; forward;
function decimal_to_binary_iterative(num: integer): string;
var
  decimal_to_binary_iterative_negative: boolean;
  decimal_to_binary_iterative_n: integer;
  decimal_to_binary_iterative_result_: string;
begin
  if num = 0 then begin
  exit('0b0');
end;
  decimal_to_binary_iterative_negative := false;
  decimal_to_binary_iterative_n := num;
  if decimal_to_binary_iterative_n < 0 then begin
  decimal_to_binary_iterative_negative := true;
  decimal_to_binary_iterative_n := -decimal_to_binary_iterative_n;
end;
  decimal_to_binary_iterative_result_ := '';
  while decimal_to_binary_iterative_n > 0 do begin
  decimal_to_binary_iterative_result_ := IntToStr(decimal_to_binary_iterative_n mod 2) + decimal_to_binary_iterative_result_;
  decimal_to_binary_iterative_n := decimal_to_binary_iterative_n div 2;
end;
  if decimal_to_binary_iterative_negative then begin
  exit('-0b' + decimal_to_binary_iterative_result_);
end;
  exit('0b' + decimal_to_binary_iterative_result_);
end;
function decimal_to_binary_recursive_helper(n: integer): string;
var
  decimal_to_binary_recursive_helper_div_: integer;
  decimal_to_binary_recursive_helper_mod_: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  if n = 1 then begin
  exit('1');
end;
  decimal_to_binary_recursive_helper_div_ := n div 2;
  decimal_to_binary_recursive_helper_mod_ := n mod 2;
  exit(decimal_to_binary_recursive_helper(decimal_to_binary_recursive_helper_div_) + IntToStr(decimal_to_binary_recursive_helper_mod_));
end;
function decimal_to_binary_recursive(num: integer): string;
begin
  if num = 0 then begin
  exit('0b0');
end;
  if num < 0 then begin
  exit('-0b' + decimal_to_binary_recursive_helper(-num));
end;
  exit('0b' + decimal_to_binary_recursive_helper(num));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(decimal_to_binary_iterative(0));
  writeln(decimal_to_binary_iterative(2));
  writeln(decimal_to_binary_iterative(7));
  writeln(decimal_to_binary_iterative(35));
  writeln(decimal_to_binary_iterative(-2));
  writeln(decimal_to_binary_recursive(0));
  writeln(decimal_to_binary_recursive(40));
  writeln(decimal_to_binary_recursive(-40));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
