{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  MATRIX_2: array of string;
  result_: integer;
  matrix_str: StrArray;
  x: integer;
  n: integer;
  row_str: string;
function parse_row(row_str: string): IntArray; forward;
function parse_matrix(matrix_str: StrArray): IntArrayArray; forward;
function bitcount(x: integer): integer; forward;
function build_powers(n: integer): IntArray; forward;
function solution(matrix_str: StrArray): integer; forward;
function parse_row(row_str: string): IntArray;
var
  parse_row_nums: array of integer;
  parse_row_current: integer;
  parse_row_has_digit: boolean;
  parse_row_i: integer;
  parse_row_ch: string;
begin
  parse_row_nums := [];
  parse_row_current := 0;
  parse_row_has_digit := false;
  parse_row_i := 0;
  while parse_row_i < Length(row_str) do begin
  parse_row_ch := copy(row_str, parse_row_i+1, (parse_row_i + 1 - (parse_row_i)));
  if parse_row_ch = ' ' then begin
  if parse_row_has_digit then begin
  parse_row_nums := concat(parse_row_nums, IntArray([parse_row_current]));
  parse_row_current := 0;
  parse_row_has_digit := false;
end;
end else begin
  parse_row_current := (parse_row_current * 10) + StrToInt(parse_row_ch);
  parse_row_has_digit := true;
end;
  parse_row_i := parse_row_i + 1;
end;
  if parse_row_has_digit then begin
  parse_row_nums := concat(parse_row_nums, IntArray([parse_row_current]));
end;
  exit(parse_row_nums);
end;
function parse_matrix(matrix_str: StrArray): IntArrayArray;
var
  parse_matrix_matrix: array of IntArray;
  parse_matrix_row_str: string;
  parse_matrix_row: IntArray;
begin
  parse_matrix_matrix := [];
  for parse_matrix_row_str in matrix_str do begin
  parse_matrix_row := parse_row(parse_matrix_row_str);
  parse_matrix_matrix := concat(parse_matrix_matrix, [parse_matrix_row]);
end;
  exit(parse_matrix_matrix);
end;
function bitcount(x: integer): integer;
var
  bitcount_count: integer;
  bitcount_y: integer;
begin
  bitcount_count := 0;
  bitcount_y := x;
  while bitcount_y > 0 do begin
  if (bitcount_y mod 2) = 1 then begin
  bitcount_count := bitcount_count + 1;
end;
  bitcount_y := bitcount_y div 2;
end;
  exit(bitcount_count);
end;
function build_powers(n: integer): IntArray;
var
  build_powers_powers: array of integer;
  build_powers_i: integer;
  build_powers_current: integer;
begin
  build_powers_powers := [];
  build_powers_i := 0;
  build_powers_current := 1;
  while build_powers_i <= n do begin
  build_powers_powers := concat(build_powers_powers, IntArray([build_powers_current]));
  build_powers_current := build_powers_current * 2;
  build_powers_i := build_powers_i + 1;
end;
  exit(build_powers_powers);
end;
function solution(matrix_str: StrArray): integer;
var
  solution_arr: IntArrayArray;
  solution_n: integer;
  solution_powers: IntArray;
  solution_size: integer;
  solution_dp: array of integer;
  solution_i: integer;
  solution_mask: integer;
  solution_row: integer;
  solution_col: integer;
  solution_new_mask: integer;
  solution_value: integer;
begin
  solution_arr := parse_matrix(matrix_str);
  solution_n := Length(solution_arr);
  solution_powers := build_powers(solution_n);
  solution_size := solution_powers[solution_n];
  solution_dp := [];
  solution_i := 0;
  while solution_i < solution_size do begin
  solution_dp := concat(solution_dp, IntArray([0]));
  solution_i := solution_i + 1;
end;
  solution_mask := 0;
  while solution_mask < solution_size do begin
  solution_row := bitcount(solution_mask);
  if solution_row < solution_n then begin
  solution_col := 0;
  while solution_col < solution_n do begin
  if ((solution_mask div solution_powers[solution_col]) mod 2) = 0 then begin
  solution_new_mask := solution_mask + solution_powers[solution_col];
  solution_value := solution_dp[solution_mask] + solution_arr[solution_row][solution_col];
  if solution_value > solution_dp[solution_new_mask] then begin
  solution_dp[solution_new_mask] := solution_value;
end;
end;
  solution_col := solution_col + 1;
end;
end;
  solution_mask := solution_mask + 1;
end;
  exit(solution_dp[solution_size - 1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  MATRIX_2 := ['7 53 183 439 863 497 383 563 79 973 287 63 343 169 583', '627 343 773 959 943 767 473 103 699 303 957 703 583 639 913', '447 283 463 29 23 487 463 993 119 883 327 493 423 159 743', '217 623 3 399 853 407 103 983 89 463 290 516 212 462 350', '960 376 682 962 300 780 486 502 912 800 250 346 172 812 350', '870 456 192 162 593 473 915 45 989 873 823 965 425 329 803', '973 965 905 919 133 673 665 235 509 613 673 815 165 992 326', '322 148 972 962 286 255 941 541 265 323 925 281 601 95 973', '445 721 11 525 473 65 511 164 138 672 18 428 154 448 848', '414 456 310 312 798 104 566 520 302 248 694 976 430 392 198', '184 829 373 181 631 101 969 613 840 740 778 458 284 760 390', '821 461 843 513 17 901 711 993 293 157 274 94 192 156 574', '34 124 4 878 450 476 712 914 838 669 875 299 823 329 699', '815 559 813 459 522 788 168 586 966 232 308 833 251 631 107', '813 883 451 509 615 77 281 613 459 205 380 274 302 35 805'];
  result_ := solution(MATRIX_2);
  writeln('solution() = ' + IntToStr(result_));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
