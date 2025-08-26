{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils, Math;
type Item = record
  _tag: integer;
  Int_value: int64;
  Str_value: string;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
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
  example1: ItemArray;
  example2: ItemArray;
  example3: ItemArray;
  example4: ItemArray;
function makeItem(_tag: integer; Int_value: int64; Str_value: string): Item; forward;
function makeItem(value: string): Item; forward;
function makeItem(value: int64): Item; forward;
function from_int(from_int_x: int64): Item; forward;
function from_string(from_string_s: string): Item; forward;
function item_to_string(item_to_string_it: Item): string; forward;
function alternative_list_arrange(alternative_list_arrange_first: ItemArray; alternative_list_arrange_second: ItemArray): ItemArray; forward;
function list_to_string(list_to_string_xs: ItemArray): string; forward;
function makeItem(_tag: integer; Int_value: int64; Str_value: string): Item;
begin
  Result._tag := _tag;
  Result.Int_value := Int_value;
  Result.Str_value := Str_value;
end;
function makeItem(value: string): Item;
begin
  Result.Str_value := value;
  Result._tag := 1;
end;
function makeItem(value: int64): Item;
begin
  Result.Int_value := value;
  Result._tag := 0;
end;
function from_int(from_int_x: int64): Item;
begin
  exit(makeItem(from_int_x));
end;
function from_string(from_string_s: string): Item;
begin
  exit(makeItem(from_string_s));
end;
function item_to_string(item_to_string_it: Item): string;
begin
  exit(IfThen(item_to_string_it._tag = 0, IntToStr(item_to_string_it.Int_value), item_to_string_it.Str_value));
end;
function alternative_list_arrange(alternative_list_arrange_first: ItemArray; alternative_list_arrange_second: ItemArray): ItemArray;
var
  alternative_list_arrange_len1: integer;
  alternative_list_arrange_len2: integer;
  alternative_list_arrange_abs_len: integer;
  alternative_list_arrange_result_: array of Item;
  alternative_list_arrange_i: int64;
begin
  alternative_list_arrange_len1 := Length(alternative_list_arrange_first);
  alternative_list_arrange_len2 := Length(alternative_list_arrange_second);
  if alternative_list_arrange_len1 > alternative_list_arrange_len2 then begin
  alternative_list_arrange_abs_len := alternative_list_arrange_len1;
end else begin
  alternative_list_arrange_abs_len := alternative_list_arrange_len2;
end;
  alternative_list_arrange_result_ := [];
  alternative_list_arrange_i := 0;
  while alternative_list_arrange_i < alternative_list_arrange_abs_len do begin
  if alternative_list_arrange_i < alternative_list_arrange_len1 then begin
  alternative_list_arrange_result_ := concat(alternative_list_arrange_result_, [alternative_list_arrange_first[alternative_list_arrange_i]]);
end;
  if alternative_list_arrange_i < alternative_list_arrange_len2 then begin
  alternative_list_arrange_result_ := concat(alternative_list_arrange_result_, [alternative_list_arrange_second[alternative_list_arrange_i]]);
end;
  alternative_list_arrange_i := alternative_list_arrange_i + 1;
end;
  exit(alternative_list_arrange_result_);
end;
function list_to_string(list_to_string_xs: ItemArray): string;
var
  list_to_string_s: string;
  list_to_string_i: int64;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(list_to_string_xs) do begin
  list_to_string_s := list_to_string_s + item_to_string(list_to_string_xs[list_to_string_i]);
  if list_to_string_i < (Length(list_to_string_xs) - 1) then begin
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
  writeln('');
end.
