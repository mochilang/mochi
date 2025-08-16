{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  xs: StrArray;
  lst: StrArray;
  x: string;
function contains_int(xs: IntArray; x: integer): boolean; forward;
function contains_string(xs: StrArray; x: string): boolean; forward;
function count_int(xs: IntArray; x: integer): integer; forward;
function count_string(xs: StrArray; x: string): integer; forward;
function sort_int(xs: IntArray): IntArray; forward;
function sort_string(xs: StrArray): StrArray; forward;
function mode_int(lst: IntArray): IntArray; forward;
function mode_string(lst: StrArray): StrArray; forward;
function contains_int(xs: IntArray; x: integer): boolean;
var
  contains_int_i: integer;
begin
  contains_int_i := 0;
  while contains_int_i < Length(xs) do begin
  if xs[contains_int_i] = x then begin
  exit(true);
end;
  contains_int_i := contains_int_i + 1;
end;
  exit(false);
end;
function contains_string(xs: StrArray; x: string): boolean;
var
  contains_string_i: integer;
begin
  contains_string_i := 0;
  while contains_string_i < Length(xs) do begin
  if xs[contains_string_i] = x then begin
  exit(true);
end;
  contains_string_i := contains_string_i + 1;
end;
  exit(false);
end;
function count_int(xs: IntArray; x: integer): integer;
var
  count_int_cnt: integer;
  count_int_i: integer;
begin
  count_int_cnt := 0;
  count_int_i := 0;
  while count_int_i < Length(xs) do begin
  if xs[count_int_i] = x then begin
  count_int_cnt := count_int_cnt + 1;
end;
  count_int_i := count_int_i + 1;
end;
  exit(count_int_cnt);
end;
function count_string(xs: StrArray; x: string): integer;
var
  count_string_cnt: integer;
  count_string_i: integer;
begin
  count_string_cnt := 0;
  count_string_i := 0;
  while count_string_i < Length(xs) do begin
  if xs[count_string_i] = x then begin
  count_string_cnt := count_string_cnt + 1;
end;
  count_string_i := count_string_i + 1;
end;
  exit(count_string_cnt);
end;
function sort_int(xs: IntArray): IntArray;
var
  sort_int_arr: array of integer;
  sort_int_i: integer;
  sort_int_j: integer;
  sort_int_tmp: integer;
begin
  sort_int_arr := xs;
  sort_int_i := 0;
  while sort_int_i < Length(sort_int_arr) do begin
  sort_int_j := sort_int_i + 1;
  while sort_int_j < Length(sort_int_arr) do begin
  if sort_int_arr[sort_int_j] < sort_int_arr[sort_int_i] then begin
  sort_int_tmp := sort_int_arr[sort_int_i];
  sort_int_arr[sort_int_i] := sort_int_arr[sort_int_j];
  sort_int_arr[sort_int_j] := sort_int_tmp;
end;
  sort_int_j := sort_int_j + 1;
end;
  sort_int_i := sort_int_i + 1;
end;
  exit(sort_int_arr);
end;
function sort_string(xs: StrArray): StrArray;
var
  sort_string_arr: array of string;
  sort_string_i: integer;
  sort_string_j: integer;
  sort_string_tmp: string;
begin
  sort_string_arr := xs;
  sort_string_i := 0;
  while sort_string_i < Length(sort_string_arr) do begin
  sort_string_j := sort_string_i + 1;
  while sort_string_j < Length(sort_string_arr) do begin
  if sort_string_arr[sort_string_j] < sort_string_arr[sort_string_i] then begin
  sort_string_tmp := sort_string_arr[sort_string_i];
  sort_string_arr[sort_string_i] := sort_string_arr[sort_string_j];
  sort_string_arr[sort_string_j] := sort_string_tmp;
end;
  sort_string_j := sort_string_j + 1;
end;
  sort_string_i := sort_string_i + 1;
end;
  exit(sort_string_arr);
end;
function mode_int(lst: IntArray): IntArray;
var
  mode_int_counts: array of integer;
  mode_int_i: integer;
  mode_int_max_count: integer;
  mode_int_modes: array of integer;
  mode_int_v: integer;
begin
  if Length(lst) = 0 then begin
  exit([]);
end;
  mode_int_counts := [];
  mode_int_i := 0;
  while mode_int_i < Length(lst) do begin
  mode_int_counts := concat(mode_int_counts, IntArray([count_int(lst, lst[mode_int_i])]));
  mode_int_i := mode_int_i + 1;
end;
  mode_int_max_count := 0;
  mode_int_i := 0;
  while mode_int_i < Length(mode_int_counts) do begin
  if mode_int_counts[mode_int_i] > mode_int_max_count then begin
  mode_int_max_count := mode_int_counts[mode_int_i];
end;
  mode_int_i := mode_int_i + 1;
end;
  mode_int_modes := [];
  mode_int_i := 0;
  while mode_int_i < Length(lst) do begin
  if mode_int_counts[mode_int_i] = mode_int_max_count then begin
  mode_int_v := lst[mode_int_i];
  if not contains_int(mode_int_modes, mode_int_v) then begin
  mode_int_modes := concat(mode_int_modes, IntArray([mode_int_v]));
end;
end;
  mode_int_i := mode_int_i + 1;
end;
  exit(sort_int(mode_int_modes));
end;
function mode_string(lst: StrArray): StrArray;
var
  mode_string_counts: array of integer;
  mode_string_i: integer;
  mode_string_max_count: integer;
  mode_string_modes: array of string;
  mode_string_v: string;
begin
  if Length(lst) = 0 then begin
  exit([]);
end;
  mode_string_counts := [];
  mode_string_i := 0;
  while mode_string_i < Length(lst) do begin
  mode_string_counts := concat(mode_string_counts, IntArray([count_string(lst, lst[mode_string_i])]));
  mode_string_i := mode_string_i + 1;
end;
  mode_string_max_count := 0;
  mode_string_i := 0;
  while mode_string_i < Length(mode_string_counts) do begin
  if mode_string_counts[mode_string_i] > mode_string_max_count then begin
  mode_string_max_count := mode_string_counts[mode_string_i];
end;
  mode_string_i := mode_string_i + 1;
end;
  mode_string_modes := [];
  mode_string_i := 0;
  while mode_string_i < Length(lst) do begin
  if mode_string_counts[mode_string_i] = mode_string_max_count then begin
  mode_string_v := lst[mode_string_i];
  if not contains_string(mode_string_modes, mode_string_v) then begin
  mode_string_modes := concat(mode_string_modes, StrArray([mode_string_v]));
end;
end;
  mode_string_i := mode_string_i + 1;
end;
  exit(sort_string(mode_string_modes));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(mode_int([2, 3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 2, 2, 2]));
  show_list(mode_int([3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 2, 2, 2]));
  show_list(mode_int([3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2]));
  writeln(list_to_str(mode_string(['x', 'y', 'y', 'z'])));
  writeln(list_to_str(mode_string(['x', 'x', 'y', 'y', 'z'])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
