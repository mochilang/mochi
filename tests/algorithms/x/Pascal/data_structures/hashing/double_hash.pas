{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
function map_int_int_to_str(m: specialize TFPGMap<integer, integer>): string;
var i: integer;
begin
  Result := 'map[';
  for i := 0 to m.Count - 1 do begin
    Result := Result + IntToStr(m.Keys[i]);
    Result := Result + ':';
    Result := Result + IntToStr(m.Data[i]);
    if i < m.Count - 1 then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  key: integer;
  value: integer;
  n: integer;
  size: integer;
  prime: integer;
  values: IntArray;
  data: IntArray;
function is_prime(n: integer): boolean; forward;
function prev_prime(n: integer): integer; forward;
function create_table(size: integer): IntArray; forward;
function hash1(size: integer; key: integer): integer; forward;
function hash2(prime: integer; key: integer): integer; forward;
function insert_double_hash(values: IntArray; size: integer; prime: integer; value: integer): IntArray; forward;
function table_keys(values: IntArray): specialize TFPGMap<integer, integer>; forward;
procedure run_example(size: integer; data: IntArray); forward;
function is_prime(n: integer): boolean;
var
  is_prime_i: integer;
begin
  if n < 2 then begin
  exit(false);
end;
  is_prime_i := 2;
  while (is_prime_i * is_prime_i) <= n do begin
  if (n mod is_prime_i) = 0 then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 1;
end;
  exit(true);
end;
function prev_prime(n: integer): integer;
var
  prev_prime_p: integer;
begin
  prev_prime_p := n - 1;
  while prev_prime_p >= 2 do begin
  if is_prime(prev_prime_p) then begin
  exit(prev_prime_p);
end;
  prev_prime_p := prev_prime_p - 1;
end;
  exit(1);
end;
function create_table(size: integer): IntArray;
var
  create_table_vals: array of integer;
  create_table_i: integer;
begin
  create_table_vals := [];
  create_table_i := 0;
  while create_table_i < size do begin
  create_table_vals := concat(create_table_vals, IntArray([-1]));
  create_table_i := create_table_i + 1;
end;
  exit(create_table_vals);
end;
function hash1(size: integer; key: integer): integer;
begin
  exit(key mod size);
end;
function hash2(prime: integer; key: integer): integer;
begin
  exit(prime - (key mod prime));
end;
function insert_double_hash(values: IntArray; size: integer; prime: integer; value: integer): IntArray;
var
  insert_double_hash_vals: array of integer;
  insert_double_hash_idx: integer;
  insert_double_hash_step: integer;
  insert_double_hash_count: integer;
begin
  insert_double_hash_vals := values;
  insert_double_hash_idx := hash1(size, value);
  insert_double_hash_step := hash2(prime, value);
  insert_double_hash_count := 0;
  while (insert_double_hash_vals[insert_double_hash_idx] <> -1) and (insert_double_hash_count < size) do begin
  insert_double_hash_idx := (insert_double_hash_idx + insert_double_hash_step) mod size;
  insert_double_hash_count := insert_double_hash_count + 1;
end;
  if insert_double_hash_vals[insert_double_hash_idx] = -1 then begin
  insert_double_hash_vals[insert_double_hash_idx] := value;
end;
  exit(insert_double_hash_vals);
end;
function table_keys(values: IntArray): specialize TFPGMap<integer, integer>;
var
  table_keys_res: specialize TFPGMap<integer, integer>;
  table_keys_i: integer;
begin
  table_keys_res := specialize TFPGMap<integer, integer>.Create();
  table_keys_i := 0;
  while table_keys_i < Length(values) do begin
  if values[table_keys_i] <> -1 then begin
  table_keys_res[table_keys_i] := values[table_keys_i];
end;
  table_keys_i := table_keys_i + 1;
end;
  exit(table_keys_res);
end;
procedure run_example(size: integer; data: IntArray);
var
  run_example_prime: integer;
  run_example_table: IntArray;
  run_example_i: integer;
begin
  run_example_prime := prev_prime(size);
  run_example_table := create_table(size);
  run_example_i := 0;
  while run_example_i < Length(data) do begin
  run_example_table := insert_double_hash(run_example_table, size, run_example_prime, data[run_example_i]);
  run_example_i := run_example_i + 1;
end;
  writeln(map_int_int_to_str(table_keys(run_example_table)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  run_example(3, [10, 20, 30]);
  run_example(4, [10, 20, 30]);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
