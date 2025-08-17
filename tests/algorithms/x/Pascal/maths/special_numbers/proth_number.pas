{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  exp_: integer;
  number: integer;
function pow2(pow2_exp_: integer): integer; forward;
function proth(number: integer): integer; forward;
procedure main(); forward;
function pow2(pow2_exp_: integer): integer;
var
  pow2_result_: integer;
  pow2_i: integer;
begin
  pow2_result_ := 1;
  pow2_i := 0;
  while pow2_i < exp_ do begin
  pow2_result_ := pow2_result_ * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_result_);
end;
function proth(number: integer): integer;
var
  proth_temp: integer;
  proth_pow: integer;
  proth_block_index: integer;
  proth_proth_list: array of integer;
  proth_proth_index: integer;
  proth_increment: integer;
  proth_block: integer;
  proth_i: integer;
  proth_next_val: integer;
begin
  if number < 1 then begin
  panic('Input value must be > 0');
end;
  if number = 1 then begin
  exit(3);
end;
  if number = 2 then begin
  exit(5);
end;
  proth_temp := Trunc(number div 3);
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
  exit(proth_proth_list[number - 1]);
end;
procedure main();
var
  main_n: integer;
  main_value: integer;
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
end.
