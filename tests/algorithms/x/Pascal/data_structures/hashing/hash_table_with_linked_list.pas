{$mode objfpc}
program Main;
uses SysUtils, fgl;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type HashTableWithLinkedList = record
  size_table: integer;
  charge_factor: integer;
  values: array of IntArray;
  keys: specialize TFPGMap<integer, IntArray>;
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
  value: integer;
  size_table: integer;
  ht: HashTableWithLinkedList;
  lst: IntArray;
  key: integer;
  data: integer;
  charge_factor: integer;
function Map1(): specialize TFPGMap<integer, IntArray>; forward;
function makeHashTableWithLinkedList(size_table: integer; charge_factor: integer; values: IntArrayArray; keys: specialize TFPGMap<integer, IntArray>): HashTableWithLinkedList; forward;
function make_table(size_table: integer; charge_factor: integer): HashTableWithLinkedList; forward;
function hash_function(ht: HashTableWithLinkedList; key: integer): integer; forward;
function prepend(lst: IntArray; value: integer): IntArray; forward;
procedure set_value(ht: HashTableWithLinkedList; key: integer; data: integer); forward;
function count_empty(ht: HashTableWithLinkedList): integer; forward;
function balanced_factor(ht: HashTableWithLinkedList): real; forward;
function collision_resolution(ht: HashTableWithLinkedList; key: integer): integer; forward;
procedure insert(ht: HashTableWithLinkedList; data: integer); forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<integer, IntArray>;
begin
  Result := specialize TFPGMap<integer, IntArray>.Create();
end;
function makeHashTableWithLinkedList(size_table: integer; charge_factor: integer; values: IntArrayArray; keys: specialize TFPGMap<integer, IntArray>): HashTableWithLinkedList;
begin
  Result.size_table := size_table;
  Result.charge_factor := charge_factor;
  Result.values := values;
  Result.keys := keys;
end;
function make_table(size_table: integer; charge_factor: integer): HashTableWithLinkedList;
var
  make_table_vals: array of IntArray;
  make_table_i: integer;
begin
  make_table_vals := [];
  make_table_i := 0;
  while make_table_i < size_table do begin
  make_table_vals := concat(make_table_vals, [[]]);
  make_table_i := make_table_i + 1;
end;
  exit(makeHashTableWithLinkedList(size_table, charge_factor, make_table_vals, Map1()));
end;
function hash_function(ht: HashTableWithLinkedList; key: integer): integer;
var
  hash_function_res: integer;
begin
  hash_function_res := key mod ht.size_table;
  if hash_function_res < 0 then begin
  hash_function_res := hash_function_res + ht.size_table;
end;
  exit(hash_function_res);
end;
function prepend(lst: IntArray; value: integer): IntArray;
var
  prepend_result_: array of integer;
  prepend_i: integer;
begin
  prepend_result_ := [value];
  prepend_i := 0;
  while prepend_i < Length(lst) do begin
  prepend_result_ := concat(prepend_result_, IntArray([lst[prepend_i]]));
  prepend_i := prepend_i + 1;
end;
  exit(prepend_result_);
end;
procedure set_value(ht: HashTableWithLinkedList; key: integer; data: integer);
var
  set_value_current: array of integer;
  set_value_updated: IntArray;
  set_value_vals: array of IntArray;
  set_value_ks: specialize TFPGMap<integer, IntArray>;
begin
  set_value_current := ht.values[key];
  set_value_updated := prepend(set_value_current, data);
  set_value_vals := ht.values;
  set_value_vals[key] := set_value_updated;
  ht.values := set_value_vals;
  set_value_ks := ht.keys;
  set_value_ks[key] := set_value_updated;
  ht.keys := set_value_ks;
end;
function count_empty(ht: HashTableWithLinkedList): integer;
var
  count_empty_count: integer;
  count_empty_i: integer;
begin
  count_empty_count := 0;
  count_empty_i := 0;
  while count_empty_i < Length(ht.values) do begin
  if Length(ht.values[count_empty_i]) = 0 then begin
  count_empty_count := count_empty_count + 1;
end;
  count_empty_i := count_empty_i + 1;
end;
  exit(count_empty_count);
end;
function balanced_factor(ht: HashTableWithLinkedList): real;
var
  balanced_factor_total: integer;
  balanced_factor_i: integer;
begin
  balanced_factor_total := 0;
  balanced_factor_i := 0;
  while balanced_factor_i < Length(ht.values) do begin
  balanced_factor_total := balanced_factor_total + (ht.charge_factor - Length(ht.values[balanced_factor_i]));
  balanced_factor_i := balanced_factor_i + 1;
end;
  exit((Double(balanced_factor_total) / Double(ht.size_table)) * Double(ht.charge_factor));
end;
function collision_resolution(ht: HashTableWithLinkedList; key: integer): integer;
var
  collision_resolution_new_key: integer;
  collision_resolution_steps: integer;
begin
  if not ((Length(ht.values[key]) = ht.charge_factor) and (count_empty(ht) = 0)) then begin
  exit(key);
end;
  collision_resolution_new_key := (key + 1) mod ht.size_table;
  collision_resolution_steps := 0;
  while (Length(ht.values[collision_resolution_new_key]) = ht.charge_factor) and (collision_resolution_steps < (ht.size_table - 1)) do begin
  collision_resolution_new_key := (collision_resolution_new_key + 1) mod ht.size_table;
  collision_resolution_steps := collision_resolution_steps + 1;
end;
  if Length(ht.values[collision_resolution_new_key]) < ht.charge_factor then begin
  exit(collision_resolution_new_key);
end;
  exit(-1);
end;
procedure insert(ht: HashTableWithLinkedList; data: integer);
var
  insert_key: integer;
  insert_dest: integer;
begin
  insert_key := hash_function(ht, data);
  if (Length(ht.values[insert_key]) = 0) or (Length(ht.values[insert_key]) < ht.charge_factor) then begin
  set_value(ht, insert_key, data);
  exit();
end;
  insert_dest := collision_resolution(ht, insert_key);
  if insert_dest >= 0 then begin
  set_value(ht, insert_dest, data);
end else begin
  writeln('table full');
end;
end;
procedure main();
var
  main_ht: HashTableWithLinkedList;
begin
  main_ht := make_table(3, 2);
  insert(main_ht, 10);
  insert(main_ht, 20);
  insert(main_ht, 30);
  insert(main_ht, 40);
  insert(main_ht, 50);
  writeln(list_list_int_to_str(main_ht.values));
  writeln(FloatToStr(balanced_factor(main_ht)));
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
