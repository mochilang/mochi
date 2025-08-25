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
procedure show_list_int64(xs: array of int64);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
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
function two_pointer(two_pointer_nums: IntArray; two_pointer_target: int64): IntArray; forward;
procedure test_two_pointer(); forward;
procedure main(); forward;
function two_pointer(two_pointer_nums: IntArray; two_pointer_target: int64): IntArray;
var
  two_pointer_i: int64;
  two_pointer_j: integer;
  two_pointer_s: int64;
begin
  two_pointer_i := 0;
  two_pointer_j := Length(two_pointer_nums) - 1;
  while two_pointer_i < two_pointer_j do begin
  two_pointer_s := two_pointer_nums[two_pointer_i] + two_pointer_nums[two_pointer_j];
  if two_pointer_s = two_pointer_target then begin
  exit([two_pointer_i, two_pointer_j]);
end;
  if two_pointer_s < two_pointer_target then begin
  two_pointer_i := two_pointer_i + 1;
end else begin
  two_pointer_j := two_pointer_j - 1;
end;
end;
  exit(IntArray([]));
end;
procedure test_two_pointer();
begin
  if list_int_to_str(two_pointer([2, 7, 11, 15], 9)) <> list_int_to_str([0, 1]) then begin
  panic('case1');
end;
  if list_int_to_str(two_pointer([2, 7, 11, 15], 17)) <> list_int_to_str([0, 3]) then begin
  panic('case2');
end;
  if list_int_to_str(two_pointer([2, 7, 11, 15], 18)) <> list_int_to_str([1, 2]) then begin
  panic('case3');
end;
  if list_int_to_str(two_pointer([2, 7, 11, 15], 26)) <> list_int_to_str([2, 3]) then begin
  panic('case4');
end;
  if list_int_to_str(two_pointer([1, 3, 3], 6)) <> list_int_to_str([1, 2]) then begin
  panic('case5');
end;
  if Length(two_pointer([2, 7, 11, 15], 8)) <> 0 then begin
  panic('case6');
end;
  if Length(two_pointer([0, 3, 6, 9, 12, 15, 18, 21, 24, 27], 19)) <> 0 then begin
  panic('case7');
end;
  if Length(two_pointer([1, 2, 3], 6)) <> 0 then begin
  panic('case8');
end;
end;
procedure main();
begin
  test_two_pointer();
  show_list_int64(two_pointer([2, 7, 11, 15], 9));
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
