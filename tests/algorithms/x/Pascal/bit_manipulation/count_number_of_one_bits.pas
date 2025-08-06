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
  b: integer;
  n: integer;
  a: integer;
function bit_and(a: integer; b: integer): integer; forward;
function count_bits_kernighan(n: integer): integer; forward;
function count_bits_modulo(n: integer): integer; forward;
procedure main(); forward;
function bit_and(a: integer; b: integer): integer;
var
  bit_and_ua: integer;
  bit_and_ub: integer;
  bit_and_res: integer;
  bit_and_bit: integer;
begin
  bit_and_ua := a;
  bit_and_ub := b;
  bit_and_res := 0;
  bit_and_bit := 1;
  while (bit_and_ua > 0) or (bit_and_ub > 0) do begin
  if ((bit_and_ua mod 2) = 1) and ((bit_and_ub mod 2) = 1) then begin
  bit_and_res := bit_and_res + bit_and_bit;
end;
  bit_and_ua := Trunc(bit_and_ua div 2);
  bit_and_ub := Trunc(bit_and_ub div 2);
  bit_and_bit := bit_and_bit * 2;
end;
  exit(bit_and_res);
end;
function count_bits_kernighan(n: integer): integer;
var
  count_bits_kernighan_num: integer;
  count_bits_kernighan_result_: integer;
begin
  if n < 0 then begin
  panic('the value of input must not be negative');
end;
  count_bits_kernighan_num := n;
  count_bits_kernighan_result_ := 0;
  while count_bits_kernighan_num <> 0 do begin
  count_bits_kernighan_num := bit_and(count_bits_kernighan_num, count_bits_kernighan_num - 1);
  count_bits_kernighan_result_ := count_bits_kernighan_result_ + 1;
end;
  exit(count_bits_kernighan_result_);
end;
function count_bits_modulo(n: integer): integer;
var
  count_bits_modulo_num: integer;
  count_bits_modulo_result_: integer;
begin
  if n < 0 then begin
  panic('the value of input must not be negative');
end;
  count_bits_modulo_num := n;
  count_bits_modulo_result_ := 0;
  while count_bits_modulo_num <> 0 do begin
  if (count_bits_modulo_num mod 2) = 1 then begin
  count_bits_modulo_result_ := count_bits_modulo_result_ + 1;
end;
  count_bits_modulo_num := Trunc(count_bits_modulo_num div 2);
end;
  exit(count_bits_modulo_result_);
end;
procedure main();
var
  main_numbers: array of integer;
  main_i: integer;
begin
  main_numbers := [25, 37, 21, 58, 0, 256];
  main_i := 0;
  while main_i < Length(main_numbers) do begin
  writeln(IntToStr(count_bits_kernighan(main_numbers[main_i])));
  main_i := main_i + 1;
end;
  main_i := 0;
  while main_i < Length(main_numbers) do begin
  writeln(IntToStr(count_bits_modulo(main_numbers[main_i])));
  main_i := main_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
