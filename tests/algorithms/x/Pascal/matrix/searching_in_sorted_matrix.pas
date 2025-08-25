{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
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
procedure search_in_sorted_matrix(search_in_sorted_matrix_mat: RealArrayArray; search_in_sorted_matrix_m: int64; search_in_sorted_matrix_n: int64; search_in_sorted_matrix_key: real); forward;
procedure main(); forward;
procedure search_in_sorted_matrix(search_in_sorted_matrix_mat: RealArrayArray; search_in_sorted_matrix_m: int64; search_in_sorted_matrix_n: int64; search_in_sorted_matrix_key: real);
var
  search_in_sorted_matrix_i: int64;
  search_in_sorted_matrix_j: int64;
begin
  search_in_sorted_matrix_i := search_in_sorted_matrix_m - 1;
  search_in_sorted_matrix_j := 0;
  while (search_in_sorted_matrix_i >= 0) and (search_in_sorted_matrix_j < search_in_sorted_matrix_n) do begin
  if search_in_sorted_matrix_key = search_in_sorted_matrix_mat[search_in_sorted_matrix_i][search_in_sorted_matrix_j] then begin
  writeln((((('Key ' + FloatToStr(search_in_sorted_matrix_key)) + ' found at row- ') + IntToStr(search_in_sorted_matrix_i + 1)) + ' column- ') + IntToStr(search_in_sorted_matrix_j + 1));
  exit();
end;
  if search_in_sorted_matrix_key < search_in_sorted_matrix_mat[search_in_sorted_matrix_i][search_in_sorted_matrix_j] then begin
  search_in_sorted_matrix_i := search_in_sorted_matrix_i - 1;
end else begin
  search_in_sorted_matrix_j := search_in_sorted_matrix_j + 1;
end;
end;
  writeln(('Key ' + FloatToStr(search_in_sorted_matrix_key)) + ' not found');
end;
procedure main();
var
  main_mat: array of RealArray;
  main_mat2: array of RealArray;
begin
  main_mat := [[2, 5, 7], [4, 8, 13], [9, 11, 15], [12, 17, 20]];
  search_in_sorted_matrix(main_mat, Length(main_mat), Length(main_mat[0]), 5);
  search_in_sorted_matrix(main_mat, Length(main_mat), Length(main_mat[0]), 21);
  main_mat2 := [[2.1, 5, 7], [4, 8, 13], [9, 11, 15], [12, 17, 20]];
  search_in_sorted_matrix(main_mat2, Length(main_mat2), Length(main_mat2[0]), 2.1);
  search_in_sorted_matrix(main_mat2, Length(main_mat2), Length(main_mat2[0]), 2.2);
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
