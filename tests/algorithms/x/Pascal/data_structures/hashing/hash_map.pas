{$mode objfpc}
program Main;
uses SysUtils;
type Bucket = record
  state: integer;
  key: integer;
  val: integer;
end;
type HashMap = record
  buckets: array of Bucket;
  len: integer;
  cap_num: integer;
  cap_den: integer;
  initial_size: integer;
end;
type BucketArray = array of Bucket;
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
  hm: HashMap;
  key: integer;
  val: integer;
  new_size: integer;
  n: integer;
  initial_size: integer;
  ind: integer;
function makeHashMap(buckets: BucketArray; len: integer; cap_num: integer; cap_den: integer; initial_size: integer): HashMap; forward;
function makeBucket(state: integer; key: integer; val: integer): Bucket; forward;
function make_buckets(n: integer): BucketArray; forward;
function hashmap_new(initial_size: integer): HashMap; forward;
function bucket_index(hm: HashMap; key: integer): integer; forward;
function next_index(hm: HashMap; ind: integer): integer; forward;
function try_set(hm: HashMap; ind: integer; key: integer; val: integer): boolean; forward;
function is_full(hm: HashMap): boolean; forward;
function is_sparse(hm: HashMap): boolean; forward;
procedure resize(hm: HashMap; new_size: integer); forward;
procedure size_up(hm: HashMap); forward;
procedure size_down(hm: HashMap); forward;
procedure add_item(hm: HashMap; key: integer; val: integer); forward;
procedure hashmap_set(hm: HashMap; key: integer; val: integer); forward;
function hashmap_get(hm: HashMap; key: integer): integer; forward;
procedure hashmap_del(hm: HashMap; key: integer); forward;
function hashmap_len(hm: HashMap): integer; forward;
function hashmap_repr(hm: HashMap): string; forward;
function makeHashMap(buckets: BucketArray; len: integer; cap_num: integer; cap_den: integer; initial_size: integer): HashMap;
begin
  Result.buckets := buckets;
  Result.len := len;
  Result.cap_num := cap_num;
  Result.cap_den := cap_den;
  Result.initial_size := initial_size;
end;
function makeBucket(state: integer; key: integer; val: integer): Bucket;
begin
  Result.state := state;
  Result.key := key;
  Result.val := val;
end;
function make_buckets(n: integer): BucketArray;
var
  make_buckets_buckets: array of Bucket;
  make_buckets_i: integer;
begin
  make_buckets_i := 0;
  while make_buckets_i < n do begin
  make_buckets_buckets := concat(make_buckets_buckets, [makeBucket(0, 0, 0)]);
  make_buckets_i := make_buckets_i + 1;
end;
  exit(make_buckets_buckets);
end;
function hashmap_new(initial_size: integer): HashMap;
begin
  exit(makeHashMap(make_buckets(initial_size), 0, 3, 4, initial_size));
end;
function bucket_index(hm: HashMap; key: integer): integer;
var
  bucket_index_ind: integer;
begin
  bucket_index_ind := key mod Length(hm.buckets);
  if bucket_index_ind < 0 then begin
  bucket_index_ind := bucket_index_ind + Length(hm.buckets);
end;
  exit(bucket_index_ind);
end;
function next_index(hm: HashMap; ind: integer): integer;
begin
  exit((ind + 1) mod Length(hm.buckets));
end;
function try_set(hm: HashMap; ind: integer; key: integer; val: integer): boolean;
var
  try_set_buckets: array of Bucket;
  try_set_b: Bucket;
begin
  try_set_buckets := hm.buckets;
  try_set_b := try_set_buckets[ind];
  if (try_set_b.state = 0) or (try_set_b.state = 2) then begin
  try_set_buckets[ind] := makeBucket(1, key, val);
  hm.buckets := try_set_buckets;
  hm.len := hm.len + 1;
  exit(true);
end;
  if try_set_b.key = key then begin
  try_set_buckets[ind] := makeBucket(1, key, val);
  hm.buckets := try_set_buckets;
  exit(true);
end;
  exit(false);
end;
function is_full(hm: HashMap): boolean;
var
  is_full_limit: integer;
begin
  is_full_limit := (Length(hm.buckets) * hm.cap_num) div hm.cap_den;
  exit(hm.len >= is_full_limit);
end;
function is_sparse(hm: HashMap): boolean;
var
  is_sparse_limit: integer;
begin
  if Length(hm.buckets) <= hm.initial_size then begin
  exit(false);
end;
  is_sparse_limit := (Length(hm.buckets) * hm.cap_num) div (2 * hm.cap_den);
  exit(hm.len < is_sparse_limit);
end;
procedure resize(hm: HashMap; new_size: integer);
var
  resize_old: array of Bucket;
  resize_i: integer;
  resize_it: Bucket;
begin
  resize_old := hm.buckets;
  hm.buckets := make_buckets(new_size);
  hm.len := 0;
  resize_i := 0;
  while resize_i < Length(resize_old) do begin
  resize_it := resize_old[resize_i];
  if resize_it.state = 1 then begin
  add_item(hm, resize_it.key, resize_it.val);
end;
  resize_i := resize_i + 1;
end;
end;
procedure size_up(hm: HashMap);
begin
  resize(hm, Length(hm.buckets) * 2);
end;
procedure size_down(hm: HashMap);
begin
  resize(hm, Length(hm.buckets) div 2);
end;
procedure add_item(hm: HashMap; key: integer; val: integer);
var
  add_item_ind: integer;
  add_item_i: integer;
begin
  add_item_ind := bucket_index(hm, key);
  add_item_i := 0;
  while add_item_i < Length(hm.buckets) do begin
  if try_set(hm, add_item_ind, key, val) then begin
  break;
end;
  add_item_ind := next_index(hm, add_item_ind);
  add_item_i := add_item_i + 1;
end;
end;
procedure hashmap_set(hm: HashMap; key: integer; val: integer);
begin
  if is_full(hm) then begin
  size_up(hm);
end;
  add_item(hm, key, val);
end;
function hashmap_get(hm: HashMap; key: integer): integer;
var
  hashmap_get_buckets: array of Bucket;
  hashmap_get_ind: integer;
  hashmap_get_i: integer;
  hashmap_get_it: Bucket;
begin
  hashmap_get_buckets := hm.buckets;
  hashmap_get_ind := bucket_index(hm, key);
  hashmap_get_i := 0;
  while hashmap_get_i < Length(hashmap_get_buckets) do begin
  hashmap_get_it := hashmap_get_buckets[hashmap_get_ind];
  if hashmap_get_it.state = 0 then begin
  break;
end;
  if (hashmap_get_it.state = 1) and (hashmap_get_it.key = key) then begin
  exit(hashmap_get_it.val);
end;
  hashmap_get_ind := next_index(hm, hashmap_get_ind);
  hashmap_get_i := hashmap_get_i + 1;
end;
  exit(0);
end;
procedure hashmap_del(hm: HashMap; key: integer);
var
  hashmap_del_buckets: array of Bucket;
  hashmap_del_ind: integer;
  hashmap_del_i: integer;
  hashmap_del_it: Bucket;
begin
  hashmap_del_buckets := hm.buckets;
  hashmap_del_ind := bucket_index(hm, key);
  hashmap_del_i := 0;
  while hashmap_del_i < Length(hashmap_del_buckets) do begin
  hashmap_del_it := hashmap_del_buckets[hashmap_del_ind];
  if hashmap_del_it.state = 0 then begin
  writeln('KeyError: ' + IntToStr(key));
  exit();
end;
  if (hashmap_del_it.state = 1) and (hashmap_del_it.key = key) then begin
  hashmap_del_buckets[hashmap_del_ind] := makeBucket(2, 0, 0);
  hm.buckets := hashmap_del_buckets;
  hm.len := hm.len - 1;
  break;
end;
  hashmap_del_ind := next_index(hm, hashmap_del_ind);
  hashmap_del_i := hashmap_del_i + 1;
end;
  if is_sparse(hm) then begin
  size_down(hm);
end;
end;
function hashmap_len(hm: HashMap): integer;
begin
  exit(hm.len);
end;
function hashmap_repr(hm: HashMap): string;
var
  hashmap_repr_out: string;
  hashmap_repr_first: boolean;
  hashmap_repr_i: integer;
  hashmap_repr_b: Bucket;
begin
  hashmap_repr_out := 'HashMap(';
  hashmap_repr_first := true;
  hashmap_repr_i := 0;
  while hashmap_repr_i < Length(hm.buckets) do begin
  hashmap_repr_b := hm.buckets[hashmap_repr_i];
  if hashmap_repr_b.state = 1 then begin
  if not hashmap_repr_first then begin
  hashmap_repr_out := hashmap_repr_out + ', ';
end else begin
  hashmap_repr_first := false;
end;
  hashmap_repr_out := ((hashmap_repr_out + IntToStr(hashmap_repr_b.key)) + ': ') + IntToStr(hashmap_repr_b.val);
end;
  hashmap_repr_i := hashmap_repr_i + 1;
end;
  hashmap_repr_out := hashmap_repr_out + ')';
  exit(hashmap_repr_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  hm := hashmap_new(5);
  hashmap_set(hm, 1, 10);
  hashmap_set(hm, 2, 20);
  hashmap_set(hm, 3, 30);
  writeln(hashmap_repr(hm));
  writeln(IntToStr(hashmap_get(hm, 2)));
  hashmap_del(hm, 1);
  writeln(hashmap_repr(hm));
  writeln(IntToStr(hashmap_len(hm)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
