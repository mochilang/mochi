{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Entry = record
  key: integer;
  val: integer;
  freq: integer;
  order: integer;
end;
type LFUCache = record
  entries: array of Entry;
  capacity: integer;
  hits: integer;
  miss: integer;
  tick: integer;
end;
type EntryArray = array of Entry;
type GetResult = record
  cache: LFUCache;
  value: integer;
  ok: boolean;
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
  key: integer;
  cache: LFUCache;
  entries: EntryArray;
  value: integer;
  cap: integer;
function makeGetResult(cache: LFUCache; value: integer; ok: boolean): GetResult; forward;
function makeLFUCache(entries: EntryArray; capacity: integer; hits: integer; miss: integer; tick: integer): LFUCache; forward;
function makeEntry(key: integer; val: integer; freq: integer; order: integer): Entry; forward;
function lfu_new(cap: integer): LFUCache; forward;
function find_entry(entries: EntryArray; key: integer): integer; forward;
function lfu_get(cache: LFUCache; key: integer): GetResult; forward;
function remove_lfu(entries: EntryArray): EntryArray; forward;
function lfu_put(cache: LFUCache; key: integer; value: integer): LFUCache; forward;
function cache_info(cache: LFUCache): string; forward;
procedure main(); forward;
function makeGetResult(cache: LFUCache; value: integer; ok: boolean): GetResult;
begin
  Result.cache := cache;
  Result.value := value;
  Result.ok := ok;
end;
function makeLFUCache(entries: EntryArray; capacity: integer; hits: integer; miss: integer; tick: integer): LFUCache;
begin
  Result.entries := entries;
  Result.capacity := capacity;
  Result.hits := hits;
  Result.miss := miss;
  Result.tick := tick;
end;
function makeEntry(key: integer; val: integer; freq: integer; order: integer): Entry;
begin
  Result.key := key;
  Result.val := val;
  Result.freq := freq;
  Result.order := order;
end;
function lfu_new(cap: integer): LFUCache;
begin
  exit(makeLFUCache([], cap, 0, 0, 0));
end;
function find_entry(entries: EntryArray; key: integer): integer;
var
  find_entry_i: integer;
  find_entry_e: Entry;
begin
  find_entry_i := 0;
  while find_entry_i < Length(entries) do begin
  find_entry_e := entries[find_entry_i];
  if find_entry_e.key = key then begin
  exit(find_entry_i);
end;
  find_entry_i := find_entry_i + 1;
end;
  exit(0 - 1);
end;
function lfu_get(cache: LFUCache; key: integer): GetResult;
var
  lfu_get_idx: integer;
  lfu_get_new_cache: LFUCache;
  lfu_get_entries: array of Entry;
  lfu_get_e: Entry;
  lfu_get_new_tick: integer;
begin
  lfu_get_idx := find_entry(cache.entries, key);
  if lfu_get_idx = (0 - 1) then begin
  lfu_get_new_cache := makeLFUCache(cache.entries, cache.capacity, cache.hits, cache.miss + 1, cache.tick);
  exit(makeGetResult(lfu_get_new_cache, 0, false));
end;
  lfu_get_entries := cache.entries;
  lfu_get_e := lfu_get_entries[lfu_get_idx];
  lfu_get_e.freq := lfu_get_e.freq + 1;
  lfu_get_new_tick := cache.tick + 1;
  lfu_get_e.order := lfu_get_new_tick;
  lfu_get_entries[lfu_get_idx] := lfu_get_e;
  lfu_get_new_cache := makeLFUCache(lfu_get_entries, cache.capacity, cache.hits + 1, cache.miss, lfu_get_new_tick);
  exit(makeGetResult(lfu_get_new_cache, lfu_get_e.val, true));
end;
function remove_lfu(entries: EntryArray): EntryArray;
var
  remove_lfu_min_idx: integer;
  remove_lfu_i: integer;
  remove_lfu_e: Entry;
  remove_lfu_m: Entry;
  remove_lfu_res: array of Entry;
  remove_lfu_j: integer;
begin
  if Length(entries) = 0 then begin
  exit(entries);
end;
  remove_lfu_min_idx := 0;
  remove_lfu_i := 1;
  while remove_lfu_i < Length(entries) do begin
  remove_lfu_e := entries[remove_lfu_i];
  remove_lfu_m := entries[remove_lfu_min_idx];
  if (remove_lfu_e.freq < remove_lfu_m.freq) or ((remove_lfu_e.freq = remove_lfu_m.freq) and (remove_lfu_e.order < remove_lfu_m.order)) then begin
  remove_lfu_min_idx := remove_lfu_i;
end;
  remove_lfu_i := remove_lfu_i + 1;
end;
  remove_lfu_res := [];
  remove_lfu_j := 0;
  while remove_lfu_j < Length(entries) do begin
  if remove_lfu_j <> remove_lfu_min_idx then begin
  remove_lfu_res := concat(remove_lfu_res, [entries[remove_lfu_j]]);
end;
  remove_lfu_j := remove_lfu_j + 1;
end;
  exit(remove_lfu_res);
end;
function lfu_put(cache: LFUCache; key: integer; value: integer): LFUCache;
var
  lfu_put_entries: array of Entry;
  lfu_put_idx: integer;
  lfu_put_e: Entry;
  lfu_put_new_tick: integer;
  lfu_put_new_entry: Entry;
begin
  lfu_put_entries := cache.entries;
  lfu_put_idx := find_entry(lfu_put_entries, key);
  if lfu_put_idx <> (0 - 1) then begin
  lfu_put_e := lfu_put_entries[lfu_put_idx];
  lfu_put_e.val := value;
  lfu_put_e.freq := lfu_put_e.freq + 1;
  lfu_put_new_tick := cache.tick + 1;
  lfu_put_e.order := lfu_put_new_tick;
  lfu_put_entries[lfu_put_idx] := lfu_put_e;
  exit(makeLFUCache(lfu_put_entries, cache.capacity, cache.hits, cache.miss, lfu_put_new_tick));
end;
  if Length(lfu_put_entries) >= cache.capacity then begin
  lfu_put_entries := remove_lfu(lfu_put_entries);
end;
  lfu_put_new_tick := cache.tick + 1;
  lfu_put_new_entry := makeEntry(key, value, 1, lfu_put_new_tick);
  lfu_put_entries := concat(lfu_put_entries, [lfu_put_new_entry]);
  exit(makeLFUCache(lfu_put_entries, cache.capacity, cache.hits, cache.miss, lfu_put_new_tick));
end;
function cache_info(cache: LFUCache): string;
begin
  exit(((((((('CacheInfo(hits=' + IntToStr(cache.hits)) + ', misses=') + IntToStr(cache.miss)) + ', capacity=') + IntToStr(cache.capacity)) + ', current_size=') + IntToStr(Length(cache.entries))) + ')');
end;
procedure main();
var
  main_cache: LFUCache;
  main_r: GetResult;
begin
  main_cache := lfu_new(2);
  main_cache := lfu_put(main_cache, 1, 1);
  main_cache := lfu_put(main_cache, 2, 2);
  main_r := lfu_get(main_cache, 1);
  main_cache := main_r.cache;
  if main_r.ok then begin
  writeln(IntToStr(main_r.value));
end else begin
  writeln('None');
end;
  main_cache := lfu_put(main_cache, 3, 3);
  main_r := lfu_get(main_cache, 2);
  main_cache := main_r.cache;
  if main_r.ok then begin
  writeln(IntToStr(main_r.value));
end else begin
  writeln('None');
end;
  main_cache := lfu_put(main_cache, 4, 4);
  main_r := lfu_get(main_cache, 1);
  main_cache := main_r.cache;
  if main_r.ok then begin
  writeln(IntToStr(main_r.value));
end else begin
  writeln('None');
end;
  main_r := lfu_get(main_cache, 3);
  main_cache := main_r.cache;
  if main_r.ok then begin
  writeln(IntToStr(main_r.value));
end else begin
  writeln('None');
end;
  main_r := lfu_get(main_cache, 4);
  main_cache := main_r.cache;
  if main_r.ok then begin
  writeln(IntToStr(main_r.value));
end else begin
  writeln('None');
end;
  writeln(cache_info(main_cache));
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
