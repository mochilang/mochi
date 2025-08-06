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
  b: integer;
  other_number: integer;
  a: integer;
function bitwise_xor(a: integer; b: integer): integer; forward;
function bitwise_and(a: integer; b: integer): integer; forward;
function bitwise_addition_recursive(number: integer; other_number: integer): integer; forward;
function bitwise_xor(a: integer; b: integer): integer;
var
  bitwise_xor_result_: integer;
  bitwise_xor_bit: integer;
  bitwise_xor_x: integer;
  bitwise_xor_y: integer;
  bitwise_xor_ax: integer;
  bitwise_xor_by: integer;
begin
  bitwise_xor_result_ := 0;
  bitwise_xor_bit := 1;
  bitwise_xor_x := a;
  bitwise_xor_y := b;
  while (bitwise_xor_x > 0) or (bitwise_xor_y > 0) do begin
  bitwise_xor_ax := bitwise_xor_x mod 2;
  bitwise_xor_by := bitwise_xor_y mod 2;
  if ((bitwise_xor_ax + bitwise_xor_by) mod 2) = 1 then begin
  bitwise_xor_result_ := bitwise_xor_result_ + bitwise_xor_bit;
end;
  bitwise_xor_x := bitwise_xor_x div 2;
  bitwise_xor_y := bitwise_xor_y div 2;
  bitwise_xor_bit := bitwise_xor_bit * 2;
end;
  exit(bitwise_xor_result_);
end;
function bitwise_and(a: integer; b: integer): integer;
var
  bitwise_and_result_: integer;
  bitwise_and_bit: integer;
  bitwise_and_x: integer;
  bitwise_and_y: integer;
begin
  bitwise_and_result_ := 0;
  bitwise_and_bit := 1;
  bitwise_and_x := a;
  bitwise_and_y := b;
  while (bitwise_and_x > 0) and (bitwise_and_y > 0) do begin
  if ((bitwise_and_x mod 2) = 1) and ((bitwise_and_y mod 2) = 1) then begin
  bitwise_and_result_ := bitwise_and_result_ + bitwise_and_bit;
end;
  bitwise_and_x := bitwise_and_x div 2;
  bitwise_and_y := bitwise_and_y div 2;
  bitwise_and_bit := bitwise_and_bit * 2;
end;
  exit(bitwise_and_result_);
end;
function bitwise_addition_recursive(number: integer; other_number: integer): integer;
var
  bitwise_addition_recursive_bitwise_sum: integer;
  bitwise_addition_recursive_carry: integer;
begin
  if (number < 0) or (other_number < 0) then begin
  panic('Both arguments MUST be non-negative!');
end;
  bitwise_addition_recursive_bitwise_sum := bitwise_xor(number, other_number);
  bitwise_addition_recursive_carry := bitwise_and(number, other_number);
  if bitwise_addition_recursive_carry = 0 then begin
  exit(bitwise_addition_recursive_bitwise_sum);
end;
  exit(bitwise_addition_recursive(bitwise_addition_recursive_bitwise_sum, bitwise_addition_recursive_carry * 2));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(bitwise_addition_recursive(4, 5)));
  writeln(IntToStr(bitwise_addition_recursive(8, 9)));
  writeln(IntToStr(bitwise_addition_recursive(0, 4)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
