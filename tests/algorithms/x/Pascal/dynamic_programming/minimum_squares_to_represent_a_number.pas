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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function make_list(make_list_len: int64; make_list_value: int64): IntArray; forward;
function int_sqrt(int_sqrt_n: int64): int64; forward;
function minimum_squares_to_represent_a_number(minimum_squares_to_represent_a_number_number: int64): int64; forward;
function make_list(make_list_len: int64; make_list_value: int64): IntArray;
var
  make_list_arr: array of int64;
  make_list_i: int64;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < make_list_len do begin
  make_list_arr := concat(make_list_arr, IntArray([make_list_value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function int_sqrt(int_sqrt_n: int64): int64;
var
  int_sqrt_r: int64;
begin
  int_sqrt_r := 0;
  while ((int_sqrt_r + 1) * (int_sqrt_r + 1)) <= int_sqrt_n do begin
  int_sqrt_r := int_sqrt_r + 1;
end;
  exit(int_sqrt_r);
end;
function minimum_squares_to_represent_a_number(minimum_squares_to_represent_a_number_number: int64): int64;
var
  minimum_squares_to_represent_a_number_answers: IntArray;
  minimum_squares_to_represent_a_number_i: int64;
  minimum_squares_to_represent_a_number_answer: int64;
  minimum_squares_to_represent_a_number_root: int64;
  minimum_squares_to_represent_a_number_j: int64;
  minimum_squares_to_represent_a_number_current_answer: int64;
begin
  if minimum_squares_to_represent_a_number_number < 0 then begin
  panic('the value of input must not be a negative number');
end;
  if minimum_squares_to_represent_a_number_number = 0 then begin
  exit(1);
end;
  minimum_squares_to_represent_a_number_answers := make_list(minimum_squares_to_represent_a_number_number + 1, -1);
  minimum_squares_to_represent_a_number_answers[0] := 0;
  minimum_squares_to_represent_a_number_i := 1;
  while minimum_squares_to_represent_a_number_i <= minimum_squares_to_represent_a_number_number do begin
  minimum_squares_to_represent_a_number_answer := minimum_squares_to_represent_a_number_i;
  minimum_squares_to_represent_a_number_root := int_sqrt(minimum_squares_to_represent_a_number_i);
  minimum_squares_to_represent_a_number_j := 1;
  while minimum_squares_to_represent_a_number_j <= minimum_squares_to_represent_a_number_root do begin
  minimum_squares_to_represent_a_number_current_answer := 1 + minimum_squares_to_represent_a_number_answers[minimum_squares_to_represent_a_number_i - (minimum_squares_to_represent_a_number_j * minimum_squares_to_represent_a_number_j)];
  if minimum_squares_to_represent_a_number_current_answer < minimum_squares_to_represent_a_number_answer then begin
  minimum_squares_to_represent_a_number_answer := minimum_squares_to_represent_a_number_current_answer;
end;
  minimum_squares_to_represent_a_number_j := minimum_squares_to_represent_a_number_j + 1;
end;
  minimum_squares_to_represent_a_number_answers[minimum_squares_to_represent_a_number_i] := minimum_squares_to_represent_a_number_answer;
  minimum_squares_to_represent_a_number_i := minimum_squares_to_represent_a_number_i + 1;
end;
  exit(minimum_squares_to_represent_a_number_answers[minimum_squares_to_represent_a_number_number]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(minimum_squares_to_represent_a_number(25));
  writeln(minimum_squares_to_represent_a_number(21));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
