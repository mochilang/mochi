{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type LRUCache = record
  max_capacity: integer;
  store: array of string;
end;
type StrArray = array of string;
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
  lru: LRUCache;
  r: string;
  n: integer;
  cache: LRUCache;
  x: string;
  xs: StrArray;
  s: string;
function makeLRUCache(max_capacity: integer; store: StrArray): LRUCache; forward;
function new_cache(n: integer): LRUCache; forward;
function remove_element(xs: StrArray; x: string): StrArray; forward;
function refer(cache: LRUCache; x: string): LRUCache; forward;
procedure display(cache: LRUCache); forward;
function repr_item(s: string): string; forward;
function cache_repr(cache: LRUCache): string; forward;
function makeLRUCache(max_capacity: integer; store: StrArray): LRUCache;
begin
  Result.max_capacity := max_capacity;
  Result.store := store;
end;
function new_cache(n: integer): LRUCache;
var
  new_cache_cap: integer;
begin
  if n < 0 then begin
  panic('n should be an integer greater than 0.');
end;
  if n = 0 then begin
  new_cache_cap := 2147483647;
end else begin
  new_cache_cap := n;
end;
  exit(makeLRUCache(new_cache_cap, StrArray([])));
end;
function remove_element(xs: StrArray; x: string): StrArray;
var
  remove_element_res: array of string;
  remove_element_removed: boolean;
  remove_element_i: integer;
  remove_element_v: string;
begin
  remove_element_res := [];
  remove_element_removed := false;
  remove_element_i := 0;
  while remove_element_i < Length(xs) do begin
  remove_element_v := xs[remove_element_i];
  if (remove_element_removed = false) and (remove_element_v = x) then begin
  remove_element_removed := true;
end else begin
  remove_element_res := concat(remove_element_res, StrArray([remove_element_v]));
end;
  remove_element_i := remove_element_i + 1;
end;
  exit(remove_element_res);
end;
function refer(cache: LRUCache; x: string): LRUCache;
var
  refer_store: array of string;
  refer_exists: boolean;
  refer_i: integer;
  refer_new_store: array of string;
  refer_j: integer;
begin
  refer_store := cache.store;
  refer_exists := false;
  refer_i := 0;
  while refer_i < Length(refer_store) do begin
  if refer_store[refer_i] = x then begin
  refer_exists := true;
end;
  refer_i := refer_i + 1;
end;
  if refer_exists then begin
  refer_store := remove_element(refer_store, x);
end else begin
  if Length(refer_store) = cache.max_capacity then begin
  refer_new_store := [];
  refer_j := 0;
  while refer_j < (Length(refer_store) - 1) do begin
  refer_new_store := concat(refer_new_store, StrArray([refer_store[refer_j]]));
  refer_j := refer_j + 1;
end;
  refer_store := refer_new_store;
end;
end;
  refer_store := concat([x], refer_store);
  exit(makeLRUCache(cache.max_capacity, refer_store));
end;
procedure display(cache: LRUCache);
var
  display_i: integer;
begin
  display_i := 0;
  while display_i < Length(cache.store) do begin
  writeln(cache.store[display_i]);
  display_i := display_i + 1;
end;
end;
function repr_item(s: string): string;
var
  repr_item_all_digits: boolean;
  repr_item_i: integer;
  repr_item_ch: string;
begin
  repr_item_all_digits := true;
  repr_item_i := 0;
  while repr_item_i < Length(s) do begin
  repr_item_ch := s[repr_item_i+1];
  if (repr_item_ch < '0') or (repr_item_ch > '9') then begin
  repr_item_all_digits := false;
end;
  repr_item_i := repr_item_i + 1;
end;
  if repr_item_all_digits then begin
  exit(s);
end;
  exit(('''' + s) + '''');
end;
function cache_repr(cache: LRUCache): string;
var
  cache_repr_res: string;
  cache_repr_i: integer;
begin
  cache_repr_res := ('LRUCache(' + IntToStr(cache.max_capacity)) + ') => [';
  cache_repr_i := 0;
  while cache_repr_i < Length(cache.store) do begin
  cache_repr_res := cache_repr_res + repr_item(cache.store[cache_repr_i]);
  if cache_repr_i < (Length(cache.store) - 1) then begin
  cache_repr_res := cache_repr_res + ', ';
end;
  cache_repr_i := cache_repr_i + 1;
end;
  cache_repr_res := cache_repr_res + ']';
  exit(cache_repr_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lru := new_cache(4);
  lru := refer(lru, 'A');
  lru := refer(lru, '2');
  lru := refer(lru, '3');
  lru := refer(lru, 'A');
  lru := refer(lru, '4');
  lru := refer(lru, '5');
  r := cache_repr(lru);
  writeln(r);
  if r <> 'LRUCache(4) => [5, 4, ''A'', 3]' then begin
  panic('Assertion error');
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
