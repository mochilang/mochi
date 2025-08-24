{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
function odd_even_transposition(odd_even_transposition_xs: IntArray): IntArray; forward;
procedure main(); forward;
function odd_even_transposition(odd_even_transposition_xs: IntArray): IntArray;
var
  odd_even_transposition_arr: array of int64;
  odd_even_transposition_n: integer;
  odd_even_transposition_phase: int64;
  odd_even_transposition_start: int64;
  odd_even_transposition_i: int64;
  odd_even_transposition_tmp: int64;
begin
  odd_even_transposition_arr := odd_even_transposition_xs;
  odd_even_transposition_n := Length(odd_even_transposition_arr);
  odd_even_transposition_phase := 0;
  while odd_even_transposition_phase < odd_even_transposition_n do begin
  odd_even_transposition_start := IfThen((odd_even_transposition_phase mod 2) = 0, 0, 1);
  odd_even_transposition_i := odd_even_transposition_start;
  while (odd_even_transposition_i + 1) < odd_even_transposition_n do begin
  if odd_even_transposition_arr[odd_even_transposition_i] > odd_even_transposition_arr[odd_even_transposition_i + 1] then begin
  odd_even_transposition_tmp := odd_even_transposition_arr[odd_even_transposition_i];
  odd_even_transposition_arr[odd_even_transposition_i] := odd_even_transposition_arr[odd_even_transposition_i + 1];
  odd_even_transposition_arr[odd_even_transposition_i + 1] := odd_even_transposition_tmp;
end;
  odd_even_transposition_i := odd_even_transposition_i + 2;
end;
  odd_even_transposition_phase := odd_even_transposition_phase + 1;
end;
  exit(odd_even_transposition_arr);
end;
procedure main();
var
  main_data: array of int64;
  main_sorted: IntArray;
begin
  main_data := [10, 9, 8, 7, 6, 5, 4, 3, 2, 1];
  writeln('Initial List');
  writeln(list_int_to_str(main_data));
  main_sorted := odd_even_transposition(main_data);
  writeln('Sorted List');
  writeln(list_int_to_str(main_sorted));
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
