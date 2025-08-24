{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
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
procedure json(x: int64);
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  ex1: array of int64;
  sorted1: array of int64;
  ex2: array of int64;
  sorted2: array of int64;
  ex3: array of int64;
  sorted3: array of int64;
  ex4: array of int64;
  sorted4: array of int64;
function get_bit_length(get_bit_length_n: int64): int64; forward;
function max_bit_length(max_bit_length_nums: IntArray): int64; forward;
function get_bit(get_bit_num: int64; get_bit_pos: int64): int64; forward;
function _msd_radix_sort(_msd_radix_sort_nums: IntArray; _msd_radix_sort_bit_position: int64): IntArray; forward;
function msd_radix_sort(msd_radix_sort_nums: IntArray): IntArray; forward;
function msd_radix_sort_inplace(msd_radix_sort_inplace_nums: IntArray): IntArray; forward;
function get_bit_length(get_bit_length_n: int64): int64;
var
  get_bit_length_length_: int64;
  get_bit_length_num: int64;
begin
  if get_bit_length_n = 0 then begin
  exit(1);
end;
  get_bit_length_length_ := 0;
  get_bit_length_num := get_bit_length_n;
  while get_bit_length_num > 0 do begin
  get_bit_length_length_ := get_bit_length_length_ + 1;
  get_bit_length_num := get_bit_length_num div 2;
end;
  exit(get_bit_length_length_);
end;
function max_bit_length(max_bit_length_nums: IntArray): int64;
var
  max_bit_length_i: int64;
  max_bit_length_max_len: int64;
  max_bit_length_l: int64;
begin
  max_bit_length_i := 0;
  max_bit_length_max_len := 0;
  while max_bit_length_i < Length(max_bit_length_nums) do begin
  max_bit_length_l := get_bit_length(max_bit_length_nums[max_bit_length_i]);
  if max_bit_length_l > max_bit_length_max_len then begin
  max_bit_length_max_len := max_bit_length_l;
end;
  max_bit_length_i := max_bit_length_i + 1;
end;
  exit(max_bit_length_max_len);
end;
function get_bit(get_bit_num: int64; get_bit_pos: int64): int64;
var
  get_bit_n: int64;
  get_bit_i: int64;
begin
  get_bit_n := get_bit_num;
  get_bit_i := 0;
  while get_bit_i < get_bit_pos do begin
  get_bit_n := get_bit_n div 2;
  get_bit_i := get_bit_i + 1;
end;
  exit(get_bit_n mod 2);
end;
function _msd_radix_sort(_msd_radix_sort_nums: IntArray; _msd_radix_sort_bit_position: int64): IntArray;
var
  _msd_radix_sort_zeros: array of int64;
  _msd_radix_sort_ones: array of int64;
  _msd_radix_sort_i: int64;
  _msd_radix_sort_num: int64;
  _msd_radix_sort_res: array of int64;
begin
  if (_msd_radix_sort_bit_position = 0) or (Length(_msd_radix_sort_nums) <= 1) then begin
  exit(_msd_radix_sort_nums);
end;
  _msd_radix_sort_zeros := [];
  _msd_radix_sort_ones := [];
  _msd_radix_sort_i := 0;
  while _msd_radix_sort_i < Length(_msd_radix_sort_nums) do begin
  _msd_radix_sort_num := _msd_radix_sort_nums[_msd_radix_sort_i];
  if get_bit(_msd_radix_sort_num, _msd_radix_sort_bit_position - 1) = 1 then begin
  _msd_radix_sort_ones := concat(_msd_radix_sort_ones, IntArray([_msd_radix_sort_num]));
end else begin
  _msd_radix_sort_zeros := concat(_msd_radix_sort_zeros, IntArray([_msd_radix_sort_num]));
end;
  _msd_radix_sort_i := _msd_radix_sort_i + 1;
end;
  _msd_radix_sort_zeros := _msd_radix_sort(_msd_radix_sort_zeros, _msd_radix_sort_bit_position - 1);
  _msd_radix_sort_ones := _msd_radix_sort(_msd_radix_sort_ones, _msd_radix_sort_bit_position - 1);
  _msd_radix_sort_res := _msd_radix_sort_zeros;
  _msd_radix_sort_i := 0;
  while _msd_radix_sort_i < Length(_msd_radix_sort_ones) do begin
  _msd_radix_sort_res := concat(_msd_radix_sort_res, IntArray([_msd_radix_sort_ones[_msd_radix_sort_i]]));
  _msd_radix_sort_i := _msd_radix_sort_i + 1;
end;
  exit(_msd_radix_sort_res);
end;
function msd_radix_sort(msd_radix_sort_nums: IntArray): IntArray;
var
  msd_radix_sort_i: int64;
  msd_radix_sort_bits: int64;
  msd_radix_sort_result_: array of int64;
begin
  if Length(msd_radix_sort_nums) = 0 then begin
  exit([]);
end;
  msd_radix_sort_i := 0;
  while msd_radix_sort_i < Length(msd_radix_sort_nums) do begin
  if msd_radix_sort_nums[msd_radix_sort_i] < 0 then begin
  panic('All numbers must be positive');
end;
  msd_radix_sort_i := msd_radix_sort_i + 1;
end;
  msd_radix_sort_bits := max_bit_length(msd_radix_sort_nums);
  msd_radix_sort_result_ := _msd_radix_sort(msd_radix_sort_nums, msd_radix_sort_bits);
  exit(msd_radix_sort_result_);
end;
function msd_radix_sort_inplace(msd_radix_sort_inplace_nums: IntArray): IntArray;
begin
  exit(msd_radix_sort(msd_radix_sort_inplace_nums));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ex1 := [40, 12, 1, 100, 4];
  sorted1 := msd_radix_sort(ex1);
  writeln(list_int_to_str(sorted1));
  ex2 := [];
  sorted2 := msd_radix_sort(ex2);
  writeln(list_int_to_str(sorted2));
  ex3 := [123, 345, 123, 80];
  sorted3 := msd_radix_sort(ex3);
  writeln(list_int_to_str(sorted3));
  ex4 := [1209, 834598, 1, 540402, 45];
  sorted4 := msd_radix_sort(ex4);
  writeln(list_int_to_str(sorted4));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
