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
  n: integer;
  number: integer;
function to_binary4(n: integer): string; forward;
function binary_coded_decimal(number: integer): string; forward;
function to_binary4(n: integer): string;
var
  to_binary4_result_: string;
  to_binary4_x: integer;
begin
  to_binary4_result_ := '';
  to_binary4_x := n;
  while to_binary4_x > 0 do begin
  to_binary4_result_ := IntToStr(to_binary4_x mod 2) + to_binary4_result_;
  to_binary4_x := to_binary4_x div 2;
end;
  while Length(to_binary4_result_) < 4 do begin
  to_binary4_result_ := '0' + to_binary4_result_;
end;
  exit(to_binary4_result_);
end;
function binary_coded_decimal(number: integer): string;
var
  binary_coded_decimal_n: integer;
  binary_coded_decimal_digits: string;
  binary_coded_decimal_out: string;
  binary_coded_decimal_i: integer;
  binary_coded_decimal_d: string;
  binary_coded_decimal_d_int: integer;
begin
  binary_coded_decimal_n := number;
  if binary_coded_decimal_n < 0 then begin
  binary_coded_decimal_n := 0;
end;
  binary_coded_decimal_digits := IntToStr(binary_coded_decimal_n);
  binary_coded_decimal_out := '0b';
  binary_coded_decimal_i := 0;
  while binary_coded_decimal_i < Length(binary_coded_decimal_digits) do begin
  binary_coded_decimal_d := binary_coded_decimal_digits[binary_coded_decimal_i+1];
  binary_coded_decimal_d_int := StrToInt(binary_coded_decimal_d);
  binary_coded_decimal_out := binary_coded_decimal_out + to_binary4(binary_coded_decimal_d_int);
  binary_coded_decimal_i := binary_coded_decimal_i + 1;
end;
  exit(binary_coded_decimal_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(binary_coded_decimal(-2));
  writeln(binary_coded_decimal(-1));
  writeln(binary_coded_decimal(0));
  writeln(binary_coded_decimal(3));
  writeln(binary_coded_decimal(2));
  writeln(binary_coded_decimal(12));
  writeln(binary_coded_decimal(987));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
