{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  r: integer;
  step_size: integer;
  num_people: integer;
  idx: integer;
  xs: IntArray;
function josephus_recursive(num_people: integer; step_size: integer): integer; forward;
function find_winner(num_people: integer; step_size: integer): integer; forward;
function remove_at(xs: IntArray; idx: integer): IntArray; forward;
function josephus_iterative(num_people: integer; step_size: integer): integer; forward;
function josephus_recursive(num_people: integer; step_size: integer): integer;
begin
  if (num_people <= 0) or (step_size <= 0) then begin
  panic('num_people or step_size is not a positive integer.');
end;
  if num_people = 1 then begin
  exit(0);
end;
  exit((josephus_recursive(num_people - 1, step_size) + step_size) mod num_people);
end;
function find_winner(num_people: integer; step_size: integer): integer;
begin
  exit(josephus_recursive(num_people, step_size) + 1);
end;
function remove_at(xs: IntArray; idx: integer): IntArray;
var
  remove_at_res: array of integer;
  remove_at_i: integer;
begin
  remove_at_res := [];
  remove_at_i := 0;
  while remove_at_i < Length(xs) do begin
  if remove_at_i <> idx then begin
  remove_at_res := concat(remove_at_res, IntArray([xs[remove_at_i]]));
end;
  remove_at_i := remove_at_i + 1;
end;
  exit(remove_at_res);
end;
function josephus_iterative(num_people: integer; step_size: integer): integer;
var
  josephus_iterative_circle: array of integer;
  josephus_iterative_i: integer;
  josephus_iterative_current: integer;
begin
  if (num_people <= 0) or (step_size <= 0) then begin
  panic('num_people or step_size is not a positive integer.');
end;
  josephus_iterative_circle := [];
  josephus_iterative_i := 1;
  while josephus_iterative_i <= num_people do begin
  josephus_iterative_circle := concat(josephus_iterative_circle, IntArray([josephus_iterative_i]));
  josephus_iterative_i := josephus_iterative_i + 1;
end;
  josephus_iterative_current := 0;
  while Length(josephus_iterative_circle) > 1 do begin
  josephus_iterative_current := ((josephus_iterative_current + step_size) - 1) mod Length(josephus_iterative_circle);
  josephus_iterative_circle := remove_at(josephus_iterative_circle, josephus_iterative_current);
end;
  exit(josephus_iterative_circle[0]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r := josephus_recursive(7, 3);
  writeln(IntToStr(r));
  writeln(IntToStr(find_winner(7, 3)));
  writeln(IntToStr(josephus_iterative(7, 3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
