{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Thing = record
  name: string;
  value: real;
  weight: real;
end;
type FuncType1 = function(arg0: Thing): real is nested;
type RealArray = array of real;
type ThingArray = array of Thing;
type StrArray = array of string;
type GreedyResult = record
  items: array of Thing;
  total_value: real;
end;
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
  food: array of string;
  value: array of real;
  weight: array of real;
  foods: ThingArray;
  res: GreedyResult;
function makeGreedyResult(items: ThingArray; total_value: real): GreedyResult; forward;
function makeThing(name: string; value: real; weight: real): Thing; forward;
function get_value(get_value_t: Thing): real; forward;
function get_weight(get_weight_t: Thing): real; forward;
function get_name(get_name_t: Thing): string; forward;
function value_weight(value_weight_t: Thing): real; forward;
function build_menu(build_menu_names: StrArray; build_menu_values: RealArray; build_menu_weights: RealArray): ThingArray; forward;
function sort_desc(sort_desc_items: ThingArray; sort_desc_key_func: FuncType1): ThingArray; forward;
function greedy(greedy_items: ThingArray; greedy_max_cost: real; greedy_key_func: FuncType1): GreedyResult; forward;
function thing_to_string(thing_to_string_t: Thing): string; forward;
function list_to_string(list_to_string_ts: ThingArray): string; forward;
function makeGreedyResult(items: ThingArray; total_value: real): GreedyResult;
begin
  Result.items := items;
  Result.total_value := total_value;
end;
function makeThing(name: string; value: real; weight: real): Thing;
begin
  Result.name := name;
  Result.value := value;
  Result.weight := weight;
end;
function get_value(get_value_t: Thing): real;
begin
  exit(get_value_t.value);
end;
function get_weight(get_weight_t: Thing): real;
begin
  exit(get_weight_t.weight);
end;
function get_name(get_name_t: Thing): string;
begin
  exit(get_name_t.name);
end;
function value_weight(value_weight_t: Thing): real;
begin
  exit(value_weight_t.value / value_weight_t.weight);
end;
function build_menu(build_menu_names: StrArray; build_menu_values: RealArray; build_menu_weights: RealArray): ThingArray;
var
  build_menu_menu: array of Thing;
  build_menu_i: int64;
begin
  build_menu_menu := [];
  build_menu_i := 0;
  while ((build_menu_i < Length(build_menu_values)) and (build_menu_i < Length(build_menu_names))) and (build_menu_i < Length(build_menu_weights)) do begin
  build_menu_menu := concat(build_menu_menu, [makeThing(build_menu_names[build_menu_i], build_menu_values[build_menu_i], build_menu_weights[build_menu_i])]);
  build_menu_i := build_menu_i + 1;
end;
  exit(build_menu_menu);
end;
function sort_desc(sort_desc_items: ThingArray; sort_desc_key_func: FuncType1): ThingArray;
var
  sort_desc_arr: array of Thing;
  sort_desc_i: int64;
  sort_desc_j: int64;
  sort_desc_key_item: Thing;
  sort_desc_key_val: real;
  sort_desc_k: int64;
begin
  sort_desc_arr := [];
  sort_desc_i := 0;
  while sort_desc_i < Length(sort_desc_items) do begin
  sort_desc_arr := concat(sort_desc_arr, [sort_desc_items[sort_desc_i]]);
  sort_desc_i := sort_desc_i + 1;
end;
  sort_desc_j := 1;
  while sort_desc_j < Length(sort_desc_arr) do begin
  sort_desc_key_item := sort_desc_arr[sort_desc_j];
  sort_desc_key_val := sort_desc_key_func(sort_desc_key_item);
  sort_desc_k := sort_desc_j - 1;
  while (sort_desc_k >= 0) and (sort_desc_key_func(sort_desc_arr[sort_desc_k]) < sort_desc_key_val) do begin
  sort_desc_arr[sort_desc_k + 1] := sort_desc_arr[sort_desc_k];
  sort_desc_k := sort_desc_k - 1;
end;
  sort_desc_arr[sort_desc_k + 1] := sort_desc_key_item;
  sort_desc_j := sort_desc_j + 1;
end;
  exit(sort_desc_arr);
end;
function greedy(greedy_items: ThingArray; greedy_max_cost: real; greedy_key_func: FuncType1): GreedyResult;
var
  greedy_items_copy: ThingArray;
  greedy_result_: array of Thing;
  greedy_total_value: real;
  greedy_total_cost: real;
  greedy_i: int64;
  greedy_it: Thing;
  greedy_w: real;
begin
  greedy_items_copy := sort_desc(greedy_items, greedy_key_func);
  greedy_result_ := [];
  greedy_total_value := 0;
  greedy_total_cost := 0;
  greedy_i := 0;
  while greedy_i < Length(greedy_items_copy) do begin
  greedy_it := greedy_items_copy[greedy_i];
  greedy_w := get_weight(greedy_it);
  if (greedy_total_cost + greedy_w) <= greedy_max_cost then begin
  greedy_result_ := concat(greedy_result_, [greedy_it]);
  greedy_total_cost := greedy_total_cost + greedy_w;
  greedy_total_value := greedy_total_value + get_value(greedy_it);
end;
  greedy_i := greedy_i + 1;
end;
  exit(makeGreedyResult(greedy_result_, greedy_total_value));
end;
function thing_to_string(thing_to_string_t: Thing): string;
begin
  exit(((((('Thing(' + thing_to_string_t.name) + ', ') + FloatToStr(thing_to_string_t.value)) + ', ') + FloatToStr(thing_to_string_t.weight)) + ')');
end;
function list_to_string(list_to_string_ts: ThingArray): string;
var
  list_to_string_s: string;
  list_to_string_i: int64;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(list_to_string_ts) do begin
  list_to_string_s := list_to_string_s + thing_to_string(list_to_string_ts[list_to_string_i]);
  if list_to_string_i < (Length(list_to_string_ts) - 1) then begin
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
  food := ['Burger', 'Pizza', 'Coca Cola', 'Rice', 'Sambhar', 'Chicken', 'Fries', 'Milk'];
  value := [80, 100, 60, 70, 50, 110, 90, 60];
  weight := [40, 60, 40, 70, 100, 85, 55, 70];
  foods := build_menu(food, value, weight);
  writeln(list_to_string(foods));
  res := greedy(foods, 500, @get_value);
  writeln(list_to_string(res.items));
  writeln(FloatToStr(res.total_value));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
