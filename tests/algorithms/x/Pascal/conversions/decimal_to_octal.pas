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
  base: integer;
  exp: integer;
  num: integer;
function int_pow(base: integer; exp: integer): integer; forward;
function decimal_to_octal(num: integer): string; forward;
function int_pow(base: integer; exp: integer): integer;
var
  int_pow_result_: integer;
  int_pow_i: integer;
begin
  int_pow_result_ := 1;
  int_pow_i := 0;
  while int_pow_i < exp do begin
  int_pow_result_ := int_pow_result_ * base;
  int_pow_i := int_pow_i + 1;
end;
  exit(int_pow_result_);
end;
function decimal_to_octal(num: integer): string;
var
  decimal_to_octal_octal: integer;
  decimal_to_octal_counter: integer;
  decimal_to_octal_value: integer;
  decimal_to_octal_remainder: integer;
begin
  if num = 0 then begin
  exit('0o0');
end;
  decimal_to_octal_octal := 0;
  decimal_to_octal_counter := 0;
  decimal_to_octal_value := num;
  while decimal_to_octal_value > 0 do begin
  decimal_to_octal_remainder := decimal_to_octal_value mod 8;
  decimal_to_octal_octal := decimal_to_octal_octal + (decimal_to_octal_remainder * int_pow(10, decimal_to_octal_counter));
  decimal_to_octal_counter := decimal_to_octal_counter + 1;
  decimal_to_octal_value := decimal_to_octal_value div 8;
end;
  exit('0o' + IntToStr(decimal_to_octal_octal));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(decimal_to_octal(2));
  writeln(decimal_to_octal(8));
  writeln(decimal_to_octal(65));
  writeln(decimal_to_octal(216));
  writeln(decimal_to_octal(512));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
