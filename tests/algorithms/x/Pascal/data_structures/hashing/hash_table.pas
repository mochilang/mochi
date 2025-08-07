{$mode objfpc}
program Main;
uses SysUtils;
type HashTable = record
  size_table: integer;
  values: array of integer;
  filled: array of boolean;
  charge_factor: integer;
  lim_charge: real;
end;
type BoolArray = array of boolean;
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
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  data: integer;
  n: integer;
  value: integer;
  lim_charge: real;
  size_table: integer;
  xs: BoolArray;
  charge_factor: integer;
  idx: integer;
  val: boolean;
  ht: HashTable;
  key: integer;
  factor: integer;
function makeHashTable(size_table: integer; values: IntArray; filled: BoolArray; charge_factor: integer; lim_charge: real): HashTable; forward;
function repeat_int(n: integer; val: integer): IntArray; forward;
function repeat_bool(n: integer; val: boolean): BoolArray; forward;
function set_int(xs: IntArray; idx: integer; value: integer): IntArray; forward;
function set_bool(xs: BoolArray; idx: integer; value: boolean): BoolArray; forward;
function create_table(size_table: integer; charge_factor: integer; lim_charge: real): HashTable; forward;
function hash_function(ht: HashTable; key: integer): integer; forward;
function is_prime(n: integer): boolean; forward;
function next_prime(value: integer; factor: integer): integer; forward;
function set_value(ht: HashTable; key: integer; data: integer): HashTable; forward;
function collision_resolution(ht: HashTable; key: integer): integer; forward;
function rehashing(ht: HashTable): HashTable; forward;
function insert_data(ht: HashTable; data: integer): HashTable; forward;
function keys(ht: HashTable): IntArrayArray; forward;
procedure main(); forward;
function makeHashTable(size_table: integer; values: IntArray; filled: BoolArray; charge_factor: integer; lim_charge: real): HashTable;
begin
  Result.size_table := size_table;
  Result.values := values;
  Result.filled := filled;
  Result.charge_factor := charge_factor;
  Result.lim_charge := lim_charge;
end;
function repeat_int(n: integer; val: integer): IntArray;
var
  repeat_int_res: array of integer;
  repeat_int_i: integer;
begin
  repeat_int_i := 0;
  while repeat_int_i < n do begin
  repeat_int_res := concat(repeat_int_res, IntArray([val]));
  repeat_int_i := repeat_int_i + 1;
end;
  exit(repeat_int_res);
end;
function repeat_bool(n: integer; val: boolean): BoolArray;
var
  repeat_bool_res: array of boolean;
  repeat_bool_i: integer;
begin
  repeat_bool_i := 0;
  while repeat_bool_i < n do begin
  repeat_bool_res := concat(repeat_bool_res, [val]);
  repeat_bool_i := repeat_bool_i + 1;
end;
  exit(repeat_bool_res);
end;
function set_int(xs: IntArray; idx: integer; value: integer): IntArray;
var
  set_int_res: array of integer;
  set_int_i: integer;
begin
  set_int_i := 0;
  while set_int_i < Length(xs) do begin
  if set_int_i = idx then begin
  set_int_res := concat(set_int_res, IntArray([value]));
end else begin
  set_int_res := concat(set_int_res, IntArray([xs[set_int_i]]));
end;
  set_int_i := set_int_i + 1;
end;
  exit(set_int_res);
end;
function set_bool(xs: BoolArray; idx: integer; value: boolean): BoolArray;
var
  set_bool_res: array of boolean;
  set_bool_i: integer;
begin
  set_bool_i := 0;
  while set_bool_i < Length(xs) do begin
  if set_bool_i = idx then begin
  set_bool_res := concat(set_bool_res, [value]);
end else begin
  set_bool_res := concat(set_bool_res, [xs[set_bool_i]]);
end;
  set_bool_i := set_bool_i + 1;
end;
  exit(set_bool_res);
end;
function create_table(size_table: integer; charge_factor: integer; lim_charge: real): HashTable;
begin
  exit(makeHashTable(size_table, repeat_int(size_table, 0), repeat_bool(size_table, false), charge_factor, lim_charge));
end;
function hash_function(ht: HashTable; key: integer): integer;
var
  hash_function_k: integer;
begin
  hash_function_k := key mod ht.size_table;
  if hash_function_k < 0 then begin
  hash_function_k := hash_function_k + ht.size_table;
end;
  exit(hash_function_k);
end;
function is_prime(n: integer): boolean;
var
  is_prime_i: integer;
begin
  if n < 2 then begin
  exit(false);
end;
  if (n mod 2) = 0 then begin
  exit(n = 2);
end;
  is_prime_i := 3;
  while (is_prime_i * is_prime_i) <= n do begin
  if (n mod is_prime_i) = 0 then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 2;
end;
  exit(true);
end;
function next_prime(value: integer; factor: integer): integer;
var
  next_prime_candidate: integer;
begin
  next_prime_candidate := (value * factor) + 1;
  while not is_prime(next_prime_candidate) do begin
  next_prime_candidate := next_prime_candidate + 1;
end;
  exit(next_prime_candidate);
end;
function set_value(ht: HashTable; key: integer; data: integer): HashTable;
var
  set_value_new_values: IntArray;
  set_value_new_filled: BoolArray;
begin
  set_value_new_values := set_int(ht.values, key, data);
  set_value_new_filled := set_bool(ht.filled, key, true);
  exit(makeHashTable(ht.size_table, set_value_new_values, set_value_new_filled, ht.charge_factor, ht.lim_charge));
end;
function collision_resolution(ht: HashTable; key: integer): integer;
var
  collision_resolution_new_key: integer;
  collision_resolution_steps: integer;
begin
  collision_resolution_new_key := hash_function(ht, key + 1);
  collision_resolution_steps := 0;
  while ht.filled[collision_resolution_new_key] do begin
  collision_resolution_new_key := hash_function(ht, collision_resolution_new_key + 1);
  collision_resolution_steps := collision_resolution_steps + 1;
  if collision_resolution_steps >= ht.size_table then begin
  exit(-1);
end;
end;
  exit(collision_resolution_new_key);
end;
function rehashing(ht: HashTable): HashTable;
var
  rehashing_survivors: array of integer;
  rehashing_i: integer;
  rehashing_new_size: integer;
  rehashing_new_ht: HashTable;
begin
  rehashing_i := 0;
  while rehashing_i < Length(ht.values) do begin
  if ht.filled[rehashing_i] then begin
  rehashing_survivors := concat(rehashing_survivors, IntArray([ht.values[rehashing_i]]));
end;
  rehashing_i := rehashing_i + 1;
end;
  rehashing_new_size := next_prime(ht.size_table, 2);
  rehashing_new_ht := create_table(rehashing_new_size, ht.charge_factor, ht.lim_charge);
  rehashing_i := 0;
  while rehashing_i < Length(rehashing_survivors) do begin
  rehashing_new_ht := insert_data(rehashing_new_ht, rehashing_survivors[rehashing_i]);
  rehashing_i := rehashing_i + 1;
end;
  exit(rehashing_new_ht);
end;
function insert_data(ht: HashTable; data: integer): HashTable;
var
  insert_data_key: integer;
  insert_data_new_key: integer;
  insert_data_resized: HashTable;
begin
  insert_data_key := hash_function(ht, data);
  if not ht.filled[insert_data_key] then begin
  exit(set_value(ht, insert_data_key, data));
end;
  if ht.values[insert_data_key] = data then begin
  exit(ht);
end;
  insert_data_new_key := collision_resolution(ht, insert_data_key);
  if insert_data_new_key >= 0 then begin
  exit(set_value(ht, insert_data_new_key, data));
end;
  insert_data_resized := rehashing(ht);
  exit(insert_data(insert_data_resized, data));
end;
function keys(ht: HashTable): IntArrayArray;
var
  keys_res: array of IntArray;
  keys_i: integer;
begin
  keys_i := 0;
  while keys_i < Length(ht.values) do begin
  if ht.filled[keys_i] then begin
  keys_res := concat(keys_res, [[keys_i, ht.values[keys_i]]]);
end;
  keys_i := keys_i + 1;
end;
  exit(keys_res);
end;
procedure main();
var
  main_ht: HashTable;
begin
  main_ht := create_table(3, 1, 0.75);
  main_ht := insert_data(main_ht, 17);
  main_ht := insert_data(main_ht, 18);
  main_ht := insert_data(main_ht, 99);
  show_list_list(keys(main_ht));
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
