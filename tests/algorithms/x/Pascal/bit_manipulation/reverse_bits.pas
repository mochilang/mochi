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
  number: integer;
function get_reverse_bit_string(number: integer): string; forward;
function reverse_bit(number: integer): string; forward;
function get_reverse_bit_string(number: integer): string;
var
  get_reverse_bit_string_bit_string: string;
  get_reverse_bit_string_n: integer;
  get_reverse_bit_string_i: integer;
begin
  get_reverse_bit_string_bit_string := '';
  get_reverse_bit_string_n := number;
  get_reverse_bit_string_i := 0;
  while get_reverse_bit_string_i < 32 do begin
  get_reverse_bit_string_bit_string := get_reverse_bit_string_bit_string + IntToStr(get_reverse_bit_string_n mod 2);
  get_reverse_bit_string_n := get_reverse_bit_string_n div 2;
  get_reverse_bit_string_i := get_reverse_bit_string_i + 1;
end;
  exit(get_reverse_bit_string_bit_string);
end;
function reverse_bit(number: integer): string;
var
  reverse_bit_n: integer;
  reverse_bit_result_: integer;
  reverse_bit_i: integer;
  reverse_bit_end_bit: integer;
begin
  if number < 0 then begin
  panic('the value of input must be positive');
end;
  reverse_bit_n := number;
  reverse_bit_result_ := 0;
  reverse_bit_i := 1;
  while reverse_bit_i <= 32 do begin
  reverse_bit_result_ := reverse_bit_result_ * 2;
  reverse_bit_end_bit := reverse_bit_n mod 2;
  reverse_bit_n := reverse_bit_n div 2;
  reverse_bit_result_ := reverse_bit_result_ + reverse_bit_end_bit;
  reverse_bit_i := reverse_bit_i + 1;
end;
  exit(get_reverse_bit_string(reverse_bit_result_));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(reverse_bit(25));
  writeln(reverse_bit(37));
  writeln(reverse_bit(21));
  writeln(reverse_bit(58));
  writeln(reverse_bit(0));
  writeln(reverse_bit(256));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
