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
  roman_values: array of integer;
  roman_symbols: array of string;
  roman: string;
  c: string;
  number: integer;
function char_value(c: string): integer; forward;
function roman_to_int(roman: string): integer; forward;
function int_to_roman(number: integer): string; forward;
function char_value(c: string): integer;
begin
  if c = 'I' then begin
  exit(1);
end;
  if c = 'V' then begin
  exit(5);
end;
  if c = 'X' then begin
  exit(10);
end;
  if c = 'L' then begin
  exit(50);
end;
  if c = 'C' then begin
  exit(100);
end;
  if c = 'D' then begin
  exit(500);
end;
  if c = 'M' then begin
  exit(1000);
end;
  exit(0);
end;
function roman_to_int(roman: string): integer;
var
  roman_to_int_total: integer;
  roman_to_int_i: integer;
begin
  roman_to_int_total := 0;
  roman_to_int_i := 0;
  while roman_to_int_i < Length(roman) do begin
  if ((roman_to_int_i + 1) < Length(roman)) and (char_value(roman[roman_to_int_i+1]) < char_value(roman[roman_to_int_i + 1+1])) then begin
  roman_to_int_total := (roman_to_int_total + char_value(roman[roman_to_int_i + 1+1])) - char_value(roman[roman_to_int_i+1]);
  roman_to_int_i := roman_to_int_i + 2;
end else begin
  roman_to_int_total := roman_to_int_total + char_value(roman[roman_to_int_i+1]);
  roman_to_int_i := roman_to_int_i + 1;
end;
end;
  exit(roman_to_int_total);
end;
function int_to_roman(number: integer): string;
var
  int_to_roman_num: integer;
  int_to_roman_res: string;
  int_to_roman_i: integer;
  int_to_roman_value: integer;
  int_to_roman_symbol: string;
  int_to_roman_factor: integer;
  int_to_roman_j: integer;
begin
  int_to_roman_num := number;
  int_to_roman_res := '';
  int_to_roman_i := 0;
  while int_to_roman_i < Length(roman_values) do begin
  int_to_roman_value := roman_values[int_to_roman_i];
  int_to_roman_symbol := roman_symbols[int_to_roman_i];
  int_to_roman_factor := int_to_roman_num div int_to_roman_value;
  int_to_roman_num := int_to_roman_num mod int_to_roman_value;
  int_to_roman_j := 0;
  while int_to_roman_j < int_to_roman_factor do begin
  int_to_roman_res := int_to_roman_res + int_to_roman_symbol;
  int_to_roman_j := int_to_roman_j + 1;
end;
  if int_to_roman_num = 0 then begin
  break;
end;
  int_to_roman_i := int_to_roman_i + 1;
end;
  exit(int_to_roman_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  roman_values := [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
  roman_symbols := ['M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'];
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
