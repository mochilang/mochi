{$mode objfpc}
program Main;
uses SysUtils, Math;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  A: array of integer;
  N: integer;
  st: array of integer;
  NEG_INF: integer;
  val: integer;
  b: integer;
  idx: integer;
  left: integer;
  a_10: integer;
  right: integer;
function left_child(idx: integer): integer; forward;
function right_child(idx: integer): integer; forward;
procedure build(idx: integer; left: integer; right: integer); forward;
function update_recursive(idx: integer; left: integer; right: integer; a_10: integer; b: integer; val: integer): boolean; forward;
function update(a_10: integer; b: integer; val: integer): boolean; forward;
function query_recursive(idx: integer; left: integer; right: integer; a_10: integer; b: integer): integer; forward;
function query(a_10: integer; b: integer): integer; forward;
procedure show_data(); forward;
procedure main(); forward;
function left_child(idx: integer): integer;
begin
  exit(idx * 2);
end;
function right_child(idx: integer): integer;
begin
  exit((idx * 2) + 1);
end;
procedure build(idx: integer; left: integer; right: integer);
var
  build_mid: integer;
  build_left_val: integer;
  build_right_val: integer;
begin
  if left = right then begin
  st[idx] := A[left];
end else begin
  build_mid := (left + right) div 2;
  build(left_child(idx), left, build_mid);
  build(right_child(idx), build_mid + 1, right);
  build_left_val := st[left_child(idx)];
  build_right_val := st[right_child(idx)];
  st[idx] := IfThen(build_left_val > build_right_val, build_left_val, build_right_val);
end;
end;
function update_recursive(idx: integer; left: integer; right: integer; a_10: integer; b: integer; val: integer): boolean;
var
  update_recursive_mid: integer;
  update_recursive_left_val: integer;
  update_recursive_right_val: integer;
begin
  if (right < a_10) or (left > b) then begin
  exit(true);
end;
  if left = right then begin
  st[idx] := val;
  exit(true);
end;
  update_recursive_mid := (left + right) div 2;
  update_recursive(left_child(idx), left, update_recursive_mid, a_10, b, val);
  update_recursive(right_child(idx), update_recursive_mid + 1, right, a_10, b, val);
  update_recursive_left_val := st[left_child(idx)];
  update_recursive_right_val := st[right_child(idx)];
  st[idx] := IfThen(update_recursive_left_val > update_recursive_right_val, update_recursive_left_val, update_recursive_right_val);
  exit(true);
end;
function update(a_10: integer; b: integer; val: integer): boolean;
begin
  exit(update_recursive(1, 0, N - 1, a_10 - 1, b - 1, val));
end;
function query_recursive(idx: integer; left: integer; right: integer; a_10: integer; b: integer): integer;
var
  query_recursive_mid: integer;
  query_recursive_q1: integer;
  query_recursive_q2: integer;
begin
  if (right < a_10) or (left > b) then begin
  exit(NEG_INF);
end;
  if (left >= a_10) and (right <= b) then begin
  exit(st[idx]);
end;
  query_recursive_mid := (left + right) div 2;
  query_recursive_q1 := query_recursive(left_child(idx), left, query_recursive_mid, a_10, b);
  query_recursive_q2 := query_recursive(right_child(idx), query_recursive_mid + 1, right, a_10, b);
  exit(IfThen(query_recursive_q1 > query_recursive_q2, query_recursive_q1, query_recursive_q2));
end;
function query(a_10: integer; b: integer): integer;
begin
  exit(query_recursive(1, 0, N - 1, a_10 - 1, b - 1));
end;
procedure show_data();
var
  show_data_i: integer;
  show_data_show_list: array of integer;
begin
  show_data_i := 0;
  show_data_show_list := [];
  while show_data_i < N do begin
  show_data_show_list := concat(show_data_show_list, IntArray([query(show_data_i + 1, show_data_i + 1)]));
  show_data_i := show_data_i + 1;
end;
  show_list(show_data_show_list);
end;
procedure main();
var
  main_i: integer;
begin
  A := [1, 2, -4, 7, 3, -5, 6, 11, -20, 9, 14, 15, 5, 2, -8];
  N := Length(A);
  main_i := 0;
  while main_i < (4 * N) do begin
  st := concat(st, IntArray([0]));
  main_i := main_i + 1;
end;
  if N > 0 then begin
  build(1, 0, N - 1);
end;
  writeln(query(4, 6));
  writeln(query(7, 11));
  writeln(query(7, 12));
  update(1, 3, 111);
  writeln(query(1, 15));
  update(7, 8, 235);
  show_data();
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  A := [];
  N := 0;
  st := [];
  NEG_INF := -1000000000;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
