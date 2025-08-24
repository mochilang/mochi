{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type StrArrayArray = array of StrArray;
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
  DIGITS: string;
  LOWER: string;
  UPPER: string;
  example1: array of string;
  example2: array of string;
function index_of(index_of_s: string; index_of_ch: string): int64; forward;
function is_digit(is_digit_ch: string): boolean; forward;
function to_lower(to_lower_ch: string): string; forward;
function pad_left(pad_left_s: string; pad_left_width: int64): string; forward;
function alphanum_key(alphanum_key_s: string): StrArray; forward;
function compare_keys(compare_keys_a: StrArray; compare_keys_b: StrArray): int64; forward;
function natural_sort(natural_sort_arr: StrArray): StrArray; forward;
function index_of(index_of_s: string; index_of_ch: string): int64;
var
  index_of_i: int64;
begin
  index_of_i := 0;
  while index_of_i < Length(index_of_s) do begin
  if index_of_s[index_of_i+1] = index_of_ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function is_digit(is_digit_ch: string): boolean;
begin
  exit(index_of(DIGITS, is_digit_ch) >= 0);
end;
function to_lower(to_lower_ch: string): string;
var
  to_lower_idx: int64;
begin
  to_lower_idx := index_of(UPPER, to_lower_ch);
  if to_lower_idx >= 0 then begin
  exit(copy(LOWER, to_lower_idx+1, (to_lower_idx + 1 - (to_lower_idx))));
end;
  exit(to_lower_ch);
end;
function pad_left(pad_left_s: string; pad_left_width: int64): string;
var
  pad_left_res: string;
begin
  pad_left_res := pad_left_s;
  while Length(pad_left_res) < pad_left_width do begin
  pad_left_res := '0' + pad_left_res;
end;
  exit(pad_left_res);
end;
function alphanum_key(alphanum_key_s: string): StrArray;
var
  alphanum_key_key: array of string;
  alphanum_key_i: int64;
  alphanum_key_num: string;
  alphanum_key_len_str: string;
  alphanum_key_seg: string;
begin
  alphanum_key_key := [];
  alphanum_key_i := 0;
  while alphanum_key_i < Length(alphanum_key_s) do begin
  if is_digit(alphanum_key_s[alphanum_key_i+1]) then begin
  alphanum_key_num := '';
  while (alphanum_key_i < Length(alphanum_key_s)) and is_digit(alphanum_key_s[alphanum_key_i+1]) do begin
  alphanum_key_num := alphanum_key_num + alphanum_key_s[alphanum_key_i+1];
  alphanum_key_i := alphanum_key_i + 1;
end;
  alphanum_key_len_str := pad_left(IntToStr(Length(alphanum_key_num)), 3);
  alphanum_key_key := concat(alphanum_key_key, StrArray([('#' + alphanum_key_len_str) + alphanum_key_num]));
end else begin
  alphanum_key_seg := '';
  while alphanum_key_i < Length(alphanum_key_s) do begin
  if is_digit(alphanum_key_s[alphanum_key_i+1]) then begin
  break;
end;
  alphanum_key_seg := alphanum_key_seg + to_lower(alphanum_key_s[alphanum_key_i+1]);
  alphanum_key_i := alphanum_key_i + 1;
end;
  alphanum_key_key := concat(alphanum_key_key, StrArray([alphanum_key_seg]));
end;
end;
  exit(alphanum_key_key);
end;
function compare_keys(compare_keys_a: StrArray; compare_keys_b: StrArray): int64;
var
  compare_keys_i: int64;
begin
  compare_keys_i := 0;
  while (compare_keys_i < Length(compare_keys_a)) and (compare_keys_i < Length(compare_keys_b)) do begin
  if compare_keys_a[compare_keys_i] < compare_keys_b[compare_keys_i] then begin
  exit(-1);
end;
  if compare_keys_a[compare_keys_i] > compare_keys_b[compare_keys_i] then begin
  exit(1);
end;
  compare_keys_i := compare_keys_i + 1;
end;
  if Length(compare_keys_a) < Length(compare_keys_b) then begin
  exit(-1);
end;
  if Length(compare_keys_a) > Length(compare_keys_b) then begin
  exit(1);
end;
  exit(0);
end;
function natural_sort(natural_sort_arr: StrArray): StrArray;
var
  natural_sort_res: array of string;
  natural_sort_keys: array of StrArray;
  natural_sort_k: int64;
  natural_sort_i: int64;
  natural_sort_current: string;
  natural_sort_current_key: array of string;
  natural_sort_j: int64;
begin
  natural_sort_res := [];
  natural_sort_keys := [];
  natural_sort_k := 0;
  while natural_sort_k < Length(natural_sort_arr) do begin
  natural_sort_res := concat(natural_sort_res, StrArray([natural_sort_arr[natural_sort_k]]));
  natural_sort_keys := concat(natural_sort_keys, [alphanum_key(natural_sort_arr[natural_sort_k])]);
  natural_sort_k := natural_sort_k + 1;
end;
  natural_sort_i := 1;
  while natural_sort_i < Length(natural_sort_res) do begin
  natural_sort_current := natural_sort_res[natural_sort_i];
  natural_sort_current_key := natural_sort_keys[natural_sort_i];
  natural_sort_j := natural_sort_i - 1;
  while (natural_sort_j >= 0) and (compare_keys(natural_sort_keys[natural_sort_j], natural_sort_current_key) > 0) do begin
  natural_sort_res[natural_sort_j + 1] := natural_sort_res[natural_sort_j];
  natural_sort_keys[natural_sort_j + 1] := natural_sort_keys[natural_sort_j];
  natural_sort_j := natural_sort_j - 1;
end;
  natural_sort_res[natural_sort_j + 1] := natural_sort_current;
  natural_sort_keys[natural_sort_j + 1] := natural_sort_current_key;
  natural_sort_i := natural_sort_i + 1;
end;
  exit(natural_sort_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  DIGITS := '0123456789';
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  example1 := ['2 ft 7 in', '1 ft 5 in', '10 ft 2 in', '2 ft 11 in', '7 ft 6 in'];
  writeln(list_to_str(natural_sort(example1)));
  example2 := ['Elm11', 'Elm12', 'Elm2', 'elm0', 'elm1', 'elm10', 'elm13', 'elm9'];
  writeln(list_to_str(natural_sort(example2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
