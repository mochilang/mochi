{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  value: integer;
  number: integer;
  n: integer;
  len: integer;
function make_list(len: integer; value: integer): IntArray; forward;
function int_sqrt(n: integer): integer; forward;
function minimum_squares_to_represent_a_number(number: integer): integer; forward;
function make_list(len: integer; value: integer): IntArray;
var
  make_list_arr: array of integer;
  make_list_i: integer;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < len do begin
  make_list_arr := concat(make_list_arr, IntArray([value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function int_sqrt(n: integer): integer;
var
  int_sqrt_r: integer;
begin
  int_sqrt_r := 0;
  while ((int_sqrt_r + 1) * (int_sqrt_r + 1)) <= n do begin
  int_sqrt_r := int_sqrt_r + 1;
end;
  exit(int_sqrt_r);
end;
function minimum_squares_to_represent_a_number(number: integer): integer;
var
  minimum_squares_to_represent_a_number_answers: IntArray;
  minimum_squares_to_represent_a_number_i: integer;
  minimum_squares_to_represent_a_number_answer: integer;
  minimum_squares_to_represent_a_number_root: integer;
  minimum_squares_to_represent_a_number_j: integer;
  minimum_squares_to_represent_a_number_current_answer: integer;
begin
  if number < 0 then begin
  panic('the value of input must not be a negative number');
end;
  if number = 0 then begin
  exit(1);
end;
  minimum_squares_to_represent_a_number_answers := make_list(number + 1, -1);
  minimum_squares_to_represent_a_number_answers[0] := 0;
  minimum_squares_to_represent_a_number_i := 1;
  while minimum_squares_to_represent_a_number_i <= number do begin
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
  exit(minimum_squares_to_represent_a_number_answers[number]);
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
end.
