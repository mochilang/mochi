{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type Item = record
end;
type ItemArray = array of Item;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  example1: ItemArray;
  example2: ItemArray;
  example3: ItemArray;
  example4: ItemArray;
  it: Item;
  second: ItemArray;
  x: integer;
  s: string;
  first: ItemArray;
  xs: ItemArray;
function makeItem(): Item; forward;
function from_int(x: integer): Item; forward;
function from_string(s: string): Item; forward;
function item_to_string(it: Item): string; forward;
function alternative_list_arrange(first: ItemArray; second: ItemArray): ItemArray; forward;
function list_to_string(xs: ItemArray): string; forward;
function makeItem(): Item;
begin
end;
function from_int(x: integer): Item;
begin
  exit(makeInt(x));
end;
function from_string(s: string): Item;
begin
  exit(makeStr(s));
end;
function item_to_string(it: Item): string;
begin
  exit(IfThen(it = Int(v), IntToStr(v), s));
end;
function alternative_list_arrange(first: ItemArray; second: ItemArray): ItemArray;
var
  alternative_list_arrange_len1: integer;
  alternative_list_arrange_len2: integer;
  alternative_list_arrange_abs_len: integer;
  alternative_list_arrange_result_: array of Item;
  alternative_list_arrange_i: integer;
begin
  alternative_list_arrange_len1 := Length(first);
  alternative_list_arrange_len2 := Length(second);
  if alternative_list_arrange_len1 > alternative_list_arrange_len2 then begin
  alternative_list_arrange_abs_len := alternative_list_arrange_len1;
end else begin
  alternative_list_arrange_abs_len := alternative_list_arrange_len2;
end;
  alternative_list_arrange_result_ := [];
  alternative_list_arrange_i := 0;
  while alternative_list_arrange_i < alternative_list_arrange_abs_len do begin
  if alternative_list_arrange_i < alternative_list_arrange_len1 then begin
  alternative_list_arrange_result_ := concat(alternative_list_arrange_result_, [first[alternative_list_arrange_i]]);
end;
  if alternative_list_arrange_i < alternative_list_arrange_len2 then begin
  alternative_list_arrange_result_ := concat(alternative_list_arrange_result_, [second[alternative_list_arrange_i]]);
end;
  alternative_list_arrange_i := alternative_list_arrange_i + 1;
end;
  exit(alternative_list_arrange_result_);
end;
function list_to_string(xs: ItemArray): string;
var
  list_to_string_s: string;
  list_to_string_i: integer;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(xs) do begin
  list_to_string_s := list_to_string_s + item_to_string(xs[list_to_string_i]);
  if list_to_string_i < (Length(xs) - 1) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_i := list_to_string_i + 1;
end;
  list_to_string_s := list_to_string_s + ']';
  exit(list_to_string_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := alternative_list_arrange([from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)], [from_string('A'), from_string('B'), from_string('C')]);
  writeln(list_to_string(example1));
  example2 := alternative_list_arrange([from_string('A'), from_string('B'), from_string('C')], [from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)]);
  writeln(list_to_string(example2));
  example3 := alternative_list_arrange([from_string('X'), from_string('Y'), from_string('Z')], [from_int(9), from_int(8), from_int(7), from_int(6)]);
  writeln(list_to_string(example3));
  example4 := alternative_list_arrange([from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)], ItemArray([]));
  writeln(list_to_string(example4));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
