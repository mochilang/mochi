{$mode objfpc}
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
  tasks_info: IntArrayArray;
function max_tasks(tasks_info: IntArrayArray): IntArray; forward;
procedure main(); forward;
function max_tasks(tasks_info: IntArrayArray): IntArray;
var
  max_tasks_order: array of integer;
  max_tasks_i: integer;
  max_tasks_n: integer;
  max_tasks_j: integer;
  max_tasks_tmp: integer;
  max_tasks_result_: array of integer;
  max_tasks_pos: integer;
  max_tasks_id: integer;
  max_tasks_deadline: integer;
begin
  max_tasks_order := [];
  max_tasks_i := 0;
  while max_tasks_i < Length(tasks_info) do begin
  max_tasks_order := concat(max_tasks_order, IntArray([max_tasks_i]));
  max_tasks_i := max_tasks_i + 1;
end;
  max_tasks_n := Length(max_tasks_order);
  max_tasks_i := 0;
  while max_tasks_i < max_tasks_n do begin
  max_tasks_j := max_tasks_i + 1;
  while max_tasks_j < max_tasks_n do begin
  if tasks_info[max_tasks_order[max_tasks_j]][1] > tasks_info[max_tasks_order[max_tasks_i]][1] then begin
  max_tasks_tmp := max_tasks_order[max_tasks_i];
  max_tasks_order[max_tasks_i] := max_tasks_order[max_tasks_j];
  max_tasks_order[max_tasks_j] := max_tasks_tmp;
end;
  max_tasks_j := max_tasks_j + 1;
end;
  max_tasks_i := max_tasks_i + 1;
end;
  max_tasks_result_ := [];
  max_tasks_pos := 1;
  max_tasks_i := 0;
  while max_tasks_i < max_tasks_n do begin
  max_tasks_id := max_tasks_order[max_tasks_i];
  max_tasks_deadline := tasks_info[max_tasks_id][0];
  if max_tasks_deadline >= max_tasks_pos then begin
  max_tasks_result_ := concat(max_tasks_result_, IntArray([max_tasks_id]));
end;
  max_tasks_i := max_tasks_i + 1;
  max_tasks_pos := max_tasks_pos + 1;
end;
  exit(max_tasks_result_);
end;
procedure main();
var
  main_ex1: array of IntArray;
  main_ex2: array of IntArray;
begin
  main_ex1 := [[4, 20], [1, 10], [1, 40], [1, 30]];
  main_ex2 := [[1, 10], [2, 20], [3, 30], [2, 40]];
  writeln(list_int_to_str(max_tasks(main_ex1)));
  writeln(list_int_to_str(max_tasks(main_ex2)));
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
