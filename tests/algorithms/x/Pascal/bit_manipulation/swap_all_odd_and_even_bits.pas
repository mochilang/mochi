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
  num: integer;
  before: integer;
  k: integer;
  after: integer;
function pad_left_num(n: integer): string; forward;
function to_binary(n: integer): string; forward;
function show_bits(before: integer; after: integer): string; forward;
function lshift(num: integer; k: integer): integer; forward;
function rshift(num: integer; k: integer): integer; forward;
function swap_odd_even_bits(num: integer): integer; forward;
procedure main(); forward;
function pad_left_num(n: integer): string;
var
  pad_left_num_s: string;
begin
  pad_left_num_s := IntToStr(n);
  while Length(pad_left_num_s) < 5 do begin
  pad_left_num_s := ' ' + pad_left_num_s;
end;
  exit(pad_left_num_s);
end;
function to_binary(n: integer): string;
var
  to_binary_sign: string;
  to_binary_num: integer;
  to_binary_bits: string;
  to_binary_min_width: integer;
begin
  to_binary_sign := '';
  to_binary_num := n;
  if to_binary_num < 0 then begin
  to_binary_sign := '-';
  to_binary_num := 0 - to_binary_num;
end;
  to_binary_bits := '';
  while to_binary_num > 0 do begin
  to_binary_bits := IntToStr(to_binary_num mod 2) + to_binary_bits;
  to_binary_num := (to_binary_num - (to_binary_num mod 2)) div 2;
end;
  if to_binary_bits = '' then begin
  to_binary_bits := '0';
end;
  to_binary_min_width := 8;
  while Length(to_binary_bits) < (to_binary_min_width - Length(to_binary_sign)) do begin
  to_binary_bits := '0' + to_binary_bits;
end;
  exit(to_binary_sign + to_binary_bits);
end;
function show_bits(before: integer; after: integer): string;
begin
  exit((((((pad_left_num(before) + ': ') + to_binary(before)) + '' + #10 + '') + pad_left_num(after)) + ': ') + to_binary(after));
end;
function lshift(num: integer; k: integer): integer;
var
  lshift_result_: integer;
  lshift_i: integer;
begin
  lshift_result_ := num;
  lshift_i := 0;
  while lshift_i < k do begin
  lshift_result_ := lshift_result_ * 2;
  lshift_i := lshift_i + 1;
end;
  exit(lshift_result_);
end;
function rshift(num: integer; k: integer): integer;
var
  rshift_result_: integer;
  rshift_i: integer;
begin
  rshift_result_ := num;
  rshift_i := 0;
  while rshift_i < k do begin
  rshift_result_ := (rshift_result_ - (rshift_result_ mod 2)) div 2;
  rshift_i := rshift_i + 1;
end;
  exit(rshift_result_);
end;
function swap_odd_even_bits(num: integer): integer;
var
  swap_odd_even_bits_n: integer;
  swap_odd_even_bits_result_: integer;
  swap_odd_even_bits_i: integer;
  swap_odd_even_bits_bit1: integer;
  swap_odd_even_bits_bit2: integer;
begin
  swap_odd_even_bits_n := num;
  if swap_odd_even_bits_n < 0 then begin
  swap_odd_even_bits_n := swap_odd_even_bits_n + 4294967296;
end;
  swap_odd_even_bits_result_ := 0;
  swap_odd_even_bits_i := 0;
  while swap_odd_even_bits_i < 32 do begin
  swap_odd_even_bits_bit1 := rshift(swap_odd_even_bits_n, swap_odd_even_bits_i) mod 2;
  swap_odd_even_bits_bit2 := rshift(swap_odd_even_bits_n, swap_odd_even_bits_i + 1) mod 2;
  swap_odd_even_bits_result_ := (swap_odd_even_bits_result_ + lshift(swap_odd_even_bits_bit1, swap_odd_even_bits_i + 1)) + lshift(swap_odd_even_bits_bit2, swap_odd_even_bits_i);
  swap_odd_even_bits_i := swap_odd_even_bits_i + 2;
end;
  exit(swap_odd_even_bits_result_);
end;
procedure main();
var
  main_nums: array of integer;
  main_i: integer;
  main_n: integer;
begin
  main_nums := [-1, 0, 1, 2, 3, 4, 23, 24];
  main_i := 0;
  while main_i < Length(main_nums) do begin
  main_n := main_nums[main_i];
  writeln(show_bits(main_n, swap_odd_even_bits(main_n)));
  writeln('');
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
