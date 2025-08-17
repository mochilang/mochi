{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function list_int_to_str(xs: array of integer): string;
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
  set_a: IntArray;
  set_b: IntArray;
  value: integer;
  xs: IntArray;
function contains(xs: IntArray; value: integer): boolean; forward;
function sumset(set_a: IntArray; set_b: IntArray): IntArray; forward;
procedure main(); forward;
function contains(xs: IntArray; value: integer): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = value then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function sumset(set_a: IntArray; set_b: IntArray): IntArray;
var
  sumset_result_: array of integer;
  sumset_i: integer;
  sumset_j: integer;
  sumset_s: integer;
begin
  sumset_result_ := [];
  sumset_i := 0;
  while sumset_i < Length(set_a) do begin
  sumset_j := 0;
  while sumset_j < Length(set_b) do begin
  sumset_s := set_a[sumset_i] + set_b[sumset_j];
  if not contains(sumset_result_, sumset_s) then begin
  sumset_result_ := concat(sumset_result_, IntArray([sumset_s]));
end;
  sumset_j := sumset_j + 1;
end;
  sumset_i := sumset_i + 1;
end;
  exit(sumset_result_);
end;
procedure main();
var
  main_set_a: array of integer;
  main_set_b: array of integer;
  main_set_c: array of integer;
begin
  main_set_a := [1, 2, 3];
  main_set_b := [4, 5, 6];
  writeln(list_int_to_str(sumset(main_set_a, main_set_b)));
  main_set_c := [4, 5, 6, 7];
  writeln(list_int_to_str(sumset(main_set_a, main_set_c)));
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
