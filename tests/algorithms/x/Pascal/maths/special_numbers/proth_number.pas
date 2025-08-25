{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function pow2(pow2_exp_: int64): int64; forward;
function proth(proth_number: int64): int64; forward;
procedure main(); forward;
function pow2(pow2_exp_: int64): int64;
var
  pow2_result_: int64;
  pow2_i: int64;
begin
  pow2_result_ := 1;
  pow2_i := 0;
  while pow2_i < pow2_exp_ do begin
  pow2_result_ := pow2_result_ * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_result_);
end;
function proth(proth_number: int64): int64;
var
  proth_temp: integer;
  proth_pow: int64;
  proth_block_index: int64;
  proth_proth_list: array of int64;
  proth_proth_index: int64;
  proth_increment: int64;
  proth_block: int64;
  proth_i: int64;
  proth_next_val: int64;
begin
  if proth_number < 1 then begin
  panic('Input value must be > 0');
end;
  if proth_number = 1 then begin
  exit(3);
end;
  if proth_number = 2 then begin
  exit(5);
end;
  proth_temp := Trunc(_floordiv(proth_number, 3));
  proth_pow := 1;
  proth_block_index := 1;
  while proth_pow <= proth_temp do begin
  proth_pow := proth_pow * 2;
  proth_block_index := proth_block_index + 1;
end;
  proth_proth_list := [3, 5];
  proth_proth_index := 2;
  proth_increment := 3;
  proth_block := 1;
  while proth_block < proth_block_index do begin
  proth_i := 0;
  while proth_i < proth_increment do begin
  proth_next_val := pow2(proth_block + 1) + proth_proth_list[proth_proth_index - 1];
  proth_proth_list := concat(proth_proth_list, IntArray([proth_next_val]));
  proth_proth_index := proth_proth_index + 1;
  proth_i := proth_i + 1;
end;
  proth_increment := proth_increment * 2;
  proth_block := proth_block + 1;
end;
  exit(proth_proth_list[proth_number - 1]);
end;
procedure main();
var
  main_n: int64;
  main_value: int64;
begin
  main_n := 1;
  while main_n <= 10 do begin
  main_value := proth(main_n);
  writeln((('The ' + IntToStr(main_n)) + 'th Proth number: ') + IntToStr(main_value));
  main_n := main_n + 1;
end;
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
  writeln('');
end.
