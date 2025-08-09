{$mode objfpc}
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
  len: integer;
  value: integer;
  b: integer;
  a: integer;
  number: integer;
function make_list(len: integer; value: integer): IntArray; forward;
function min_int(a: integer; b: integer): integer; forward;
function min_steps_to_one(number: integer): integer; forward;
function make_list(len: integer; value: integer): IntArray;
var
  make_list_arr: array of integer;
  make_list_i: integer;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < len do begin
  make_list_arr := concat(make_list_arr, IntArray([value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function min_steps_to_one(number: integer): integer;
var
  min_steps_to_one_table: IntArray;
  min_steps_to_one_i: integer;
begin
  if number <= 0 then begin
  exit(0);
end;
  min_steps_to_one_table := make_list(number + 1, number + 1);
  min_steps_to_one_table[1] := 0;
  min_steps_to_one_i := 1;
  while min_steps_to_one_i < number do begin
  min_steps_to_one_table[min_steps_to_one_i + 1] := min_int(min_steps_to_one_table[min_steps_to_one_i + 1], min_steps_to_one_table[min_steps_to_one_i] + 1);
  if (min_steps_to_one_i * 2) <= number then begin
  min_steps_to_one_table[min_steps_to_one_i * 2] := min_int(min_steps_to_one_table[min_steps_to_one_i * 2], min_steps_to_one_table[min_steps_to_one_i] + 1);
end;
  if (min_steps_to_one_i * 3) <= number then begin
  min_steps_to_one_table[min_steps_to_one_i * 3] := min_int(min_steps_to_one_table[min_steps_to_one_i * 3], min_steps_to_one_table[min_steps_to_one_i] + 1);
end;
  min_steps_to_one_i := min_steps_to_one_i + 1;
end;
  exit(min_steps_to_one_table[number]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(min_steps_to_one(10)));
  writeln(IntToStr(min_steps_to_one(15)));
  writeln(IntToStr(min_steps_to_one(6)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
