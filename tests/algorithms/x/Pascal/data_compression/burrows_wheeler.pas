{$mode objfpc}
program Main;
uses SysUtils;
type BWTResult = record
  bwt_string: string;
  idx_original_string: integer;
end;
type StrArray = array of string;
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
  s: string;
  result_: BWTResult;
  bwt_string: string;
  arr: StrArray;
  target: string;
  idx_original_string: integer;
function makeBWTResult(bwt_string: string; idx_original_string: integer): BWTResult; forward;
function all_rotations(s: string): StrArray; forward;
function sort_strings(arr: StrArray): StrArray; forward;
function join_strings(arr: StrArray): string; forward;
function bwt_transform(s: string): BWTResult; forward;
function index_of(arr: StrArray; target: string): integer; forward;
function reverse_bwt(bwt_string: string; idx_original_string: integer): string; forward;
function makeBWTResult(bwt_string: string; idx_original_string: integer): BWTResult;
begin
  Result.bwt_string := bwt_string;
  Result.idx_original_string := idx_original_string;
end;
function all_rotations(s: string): StrArray;
var
  all_rotations_n: integer;
  all_rotations_rotations: array of string;
  all_rotations_i: integer;
  all_rotations_rotation: string;
begin
  all_rotations_n := Length(s);
  all_rotations_rotations := [];
  all_rotations_i := 0;
  while all_rotations_i < all_rotations_n do begin
  all_rotations_rotation := copy(s, all_rotations_i+1, (all_rotations_n - (all_rotations_i))) + copy(s, 0+1, (all_rotations_i - (0)));
  all_rotations_rotations := concat(all_rotations_rotations, StrArray([all_rotations_rotation]));
  all_rotations_i := all_rotations_i + 1;
end;
  exit(all_rotations_rotations);
end;
function sort_strings(arr: StrArray): StrArray;
var
  sort_strings_n: integer;
  sort_strings_i: integer;
  sort_strings_key: string;
  sort_strings_j: integer;
begin
  sort_strings_n := Length(arr);
  sort_strings_i := 1;
  while sort_strings_i < sort_strings_n do begin
  sort_strings_key := arr[sort_strings_i];
  sort_strings_j := sort_strings_i - 1;
  while (sort_strings_j >= 0) and (arr[sort_strings_j] > sort_strings_key) do begin
  arr[sort_strings_j + 1] := arr[sort_strings_j];
  sort_strings_j := sort_strings_j - 1;
end;
  arr[sort_strings_j + 1] := sort_strings_key;
  sort_strings_i := sort_strings_i + 1;
end;
  exit(arr);
end;
function join_strings(arr: StrArray): string;
var
  join_strings_res: string;
  join_strings_i: integer;
begin
  join_strings_res := '';
  join_strings_i := 0;
  while join_strings_i < Length(arr) do begin
  join_strings_res := join_strings_res + arr[join_strings_i];
  join_strings_i := join_strings_i + 1;
end;
  exit(join_strings_res);
end;
function bwt_transform(s: string): BWTResult;
var
  bwt_transform_rotations: StrArray;
  bwt_transform_last_col: array of string;
  bwt_transform_i: integer;
  bwt_transform_word: string;
  bwt_transform_bwt_string: string;
  bwt_transform_idx: integer;
begin
  if s = '' then begin
  panic('input string must not be empty');
end;
  bwt_transform_rotations := all_rotations(s);
  bwt_transform_rotations := sort_strings(bwt_transform_rotations);
  bwt_transform_last_col := [];
  bwt_transform_i := 0;
  while bwt_transform_i < Length(bwt_transform_rotations) do begin
  bwt_transform_word := bwt_transform_rotations[bwt_transform_i];
  bwt_transform_last_col := concat(bwt_transform_last_col, StrArray([copy(bwt_transform_word, Length(bwt_transform_word) - 1+1, (Length(bwt_transform_word) - (Length(bwt_transform_word) - 1)))]));
  bwt_transform_i := bwt_transform_i + 1;
end;
  bwt_transform_bwt_string := join_strings(bwt_transform_last_col);
  bwt_transform_idx := index_of(bwt_transform_rotations, s);
  exit(makeBWTResult(bwt_transform_bwt_string, bwt_transform_idx));
end;
function index_of(arr: StrArray; target: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(arr) do begin
  if arr[index_of_i] = target then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function reverse_bwt(bwt_string: string; idx_original_string: integer): string;
var
  reverse_bwt_n: integer;
  reverse_bwt_ordered_rotations: array of string;
  reverse_bwt_i: integer;
  reverse_bwt_iter: integer;
  reverse_bwt_j: integer;
  reverse_bwt_ch: string;
begin
  if bwt_string = '' then begin
  panic('bwt string must not be empty');
end;
  reverse_bwt_n := Length(bwt_string);
  if (idx_original_string < 0) or (idx_original_string >= reverse_bwt_n) then begin
  panic('index out of range');
end;
  reverse_bwt_ordered_rotations := [];
  reverse_bwt_i := 0;
  while reverse_bwt_i < reverse_bwt_n do begin
  reverse_bwt_ordered_rotations := concat(reverse_bwt_ordered_rotations, StrArray(['']));
  reverse_bwt_i := reverse_bwt_i + 1;
end;
  reverse_bwt_iter := 0;
  while reverse_bwt_iter < reverse_bwt_n do begin
  reverse_bwt_j := 0;
  while reverse_bwt_j < reverse_bwt_n do begin
  reverse_bwt_ch := copy(bwt_string, reverse_bwt_j+1, (reverse_bwt_j + 1 - (reverse_bwt_j)));
  reverse_bwt_ordered_rotations[reverse_bwt_j] := reverse_bwt_ch + reverse_bwt_ordered_rotations[reverse_bwt_j];
  reverse_bwt_j := reverse_bwt_j + 1;
end;
  reverse_bwt_ordered_rotations := sort_strings(reverse_bwt_ordered_rotations);
  reverse_bwt_iter := reverse_bwt_iter + 1;
end;
  exit(reverse_bwt_ordered_rotations[idx_original_string]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  s := '^BANANA';
  result_ := bwt_transform(s);
  writeln(result_.bwt_string);
  writeln(result_.idx_original_string);
  writeln(reverse_bwt(result_.bwt_string, result_.idx_original_string));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
