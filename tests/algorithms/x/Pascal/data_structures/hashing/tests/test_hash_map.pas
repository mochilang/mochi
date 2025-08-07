{$mode objfpc}
program Main;
uses SysUtils;
type Entry = record
  key: string;
  value: string;
end;
type HashMap = record
  entries: array of Entry;
end;
type GetResult = record
  found: boolean;
  value: string;
end;
type DelResult = record
  map: HashMap;
  ok: boolean;
end;
type EntryArray = array of Entry;
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
  m: HashMap;
  value: string;
  key: string;
function makeDelResult(map: HashMap; ok: boolean): DelResult; forward;
function makeGetResult(found: boolean; value: string): GetResult; forward;
function makeHashMap(entries: EntryArray): HashMap; forward;
function makeEntry(key: string; value: string): Entry; forward;
function make_hash_map(): HashMap; forward;
function hm_len(m: HashMap): integer; forward;
function hm_set(m: HashMap; key: string; value: string): HashMap; forward;
function hm_get(m: HashMap; key: string): GetResult; forward;
function hm_del(m: HashMap; key: string): DelResult; forward;
function test_add_items(): boolean; forward;
function test_overwrite_items(): boolean; forward;
function test_delete_items(): boolean; forward;
function test_access_absent_items(): boolean; forward;
function test_add_with_resize_up(): boolean; forward;
function test_add_with_resize_down(): boolean; forward;
function makeDelResult(map: HashMap; ok: boolean): DelResult;
begin
  Result.map := map;
  Result.ok := ok;
end;
function makeGetResult(found: boolean; value: string): GetResult;
begin
  Result.found := found;
  Result.value := value;
end;
function makeHashMap(entries: EntryArray): HashMap;
begin
  Result.entries := entries;
end;
function makeEntry(key: string; value: string): Entry;
begin
  Result.key := key;
  Result.value := value;
end;
function make_hash_map(): HashMap;
begin
  exit(makeHashMap(EntryArray([])));
end;
function hm_len(m: HashMap): integer;
begin
  exit(Length(m.entries));
end;
function hm_set(m: HashMap; key: string; value: string): HashMap;
var
  hm_set_entries: array of Entry;
  hm_set_updated: boolean;
  hm_set_new_entries: array of Entry;
  hm_set_i: integer;
  hm_set_e: Entry;
begin
  hm_set_entries := m.entries;
  hm_set_updated := false;
  hm_set_new_entries := EntryArray([]);
  hm_set_i := 0;
  while hm_set_i < Length(hm_set_entries) do begin
  hm_set_e := hm_set_entries[hm_set_i];
  if hm_set_e.key = key then begin
  hm_set_new_entries := concat(hm_set_new_entries, [makeEntry(key, value)]);
  hm_set_updated := true;
end else begin
  hm_set_new_entries := concat(hm_set_new_entries, [hm_set_e]);
end;
  hm_set_i := hm_set_i + 1;
end;
  if not hm_set_updated then begin
  hm_set_new_entries := concat(hm_set_new_entries, [makeEntry(key, value)]);
end;
  exit(makeHashMap(hm_set_new_entries));
end;
function hm_get(m: HashMap; key: string): GetResult;
var
  hm_get_i: integer;
  hm_get_e: Entry;
begin
  hm_get_i := 0;
  while hm_get_i < Length(m.entries) do begin
  hm_get_e := m.entries[hm_get_i];
  if hm_get_e.key = key then begin
  exit(makeGetResult(true, hm_get_e.value));
end;
  hm_get_i := hm_get_i + 1;
end;
  exit(makeGetResult(false, ''));
end;
function hm_del(m: HashMap; key: string): DelResult;
var
  hm_del_entries: array of Entry;
  hm_del_new_entries: array of Entry;
  hm_del_removed: boolean;
  hm_del_i: integer;
  hm_del_e: Entry;
begin
  hm_del_entries := m.entries;
  hm_del_new_entries := EntryArray([]);
  hm_del_removed := false;
  hm_del_i := 0;
  while hm_del_i < Length(hm_del_entries) do begin
  hm_del_e := hm_del_entries[hm_del_i];
  if hm_del_e.key = key then begin
  hm_del_removed := true;
end else begin
  hm_del_new_entries := concat(hm_del_new_entries, [hm_del_e]);
end;
  hm_del_i := hm_del_i + 1;
end;
  if hm_del_removed then begin
  exit(makeDelResult(makeHashMap(hm_del_new_entries), true));
end;
  exit(makeDelResult(m, false));
end;
function test_add_items(): boolean;
var
  test_add_items_h: HashMap;
  test_add_items_a: GetResult;
  test_add_items_b: GetResult;
begin
  test_add_items_h := make_hash_map();
  test_add_items_h := hm_set(test_add_items_h, 'key_a', 'val_a');
  test_add_items_h := hm_set(test_add_items_h, 'key_b', 'val_b');
  test_add_items_a := hm_get(test_add_items_h, 'key_a');
  test_add_items_b := hm_get(test_add_items_h, 'key_b');
  exit(((((hm_len(test_add_items_h) = 2) and test_add_items_a.found) and test_add_items_b.found) and (test_add_items_a.value = 'val_a')) and (test_add_items_b.value = 'val_b'));
end;
function test_overwrite_items(): boolean;
var
  test_overwrite_items_h: HashMap;
  test_overwrite_items_a: GetResult;
begin
  test_overwrite_items_h := make_hash_map();
  test_overwrite_items_h := hm_set(test_overwrite_items_h, 'key_a', 'val_a');
  test_overwrite_items_h := hm_set(test_overwrite_items_h, 'key_a', 'val_b');
  test_overwrite_items_a := hm_get(test_overwrite_items_h, 'key_a');
  exit(((hm_len(test_overwrite_items_h) = 1) and test_overwrite_items_a.found) and (test_overwrite_items_a.value = 'val_b'));
end;
function test_delete_items(): boolean;
var
  test_delete_items_h: HashMap;
  test_delete_items_d1: DelResult;
  test_delete_items_d2: DelResult;
  test_delete_items_d3: DelResult;
begin
  test_delete_items_h := make_hash_map();
  test_delete_items_h := hm_set(test_delete_items_h, 'key_a', 'val_a');
  test_delete_items_h := hm_set(test_delete_items_h, 'key_b', 'val_b');
  test_delete_items_d1 := hm_del(test_delete_items_h, 'key_a');
  test_delete_items_h := test_delete_items_d1.map;
  test_delete_items_d2 := hm_del(test_delete_items_h, 'key_b');
  test_delete_items_h := test_delete_items_d2.map;
  test_delete_items_h := hm_set(test_delete_items_h, 'key_a', 'val_a');
  test_delete_items_d3 := hm_del(test_delete_items_h, 'key_a');
  test_delete_items_h := test_delete_items_d3.map;
  exit(hm_len(test_delete_items_h) = 0);
end;
function test_access_absent_items(): boolean;
var
  test_access_absent_items_h: HashMap;
  test_access_absent_items_g1: GetResult;
  test_access_absent_items_d1: DelResult;
  test_access_absent_items_d2: DelResult;
  test_access_absent_items_d3: DelResult;
  test_access_absent_items_g2: GetResult;
begin
  test_access_absent_items_h := make_hash_map();
  test_access_absent_items_g1 := hm_get(test_access_absent_items_h, 'key_a');
  test_access_absent_items_d1 := hm_del(test_access_absent_items_h, 'key_a');
  test_access_absent_items_h := test_access_absent_items_d1.map;
  test_access_absent_items_h := hm_set(test_access_absent_items_h, 'key_a', 'val_a');
  test_access_absent_items_d2 := hm_del(test_access_absent_items_h, 'key_a');
  test_access_absent_items_h := test_access_absent_items_d2.map;
  test_access_absent_items_d3 := hm_del(test_access_absent_items_h, 'key_a');
  test_access_absent_items_h := test_access_absent_items_d3.map;
  test_access_absent_items_g2 := hm_get(test_access_absent_items_h, 'key_a');
  exit((((((test_access_absent_items_g1.found = false) and (test_access_absent_items_d1.ok = false)) and test_access_absent_items_d2.ok) and (test_access_absent_items_d3.ok = false)) and (test_access_absent_items_g2.found = false)) and (hm_len(test_access_absent_items_h) = 0));
end;
function test_add_with_resize_up(): boolean;
var
  test_add_with_resize_up_h: HashMap;
  test_add_with_resize_up_i: integer;
  test_add_with_resize_up_s: string;
begin
  test_add_with_resize_up_h := make_hash_map();
  test_add_with_resize_up_i := 0;
  while test_add_with_resize_up_i < 5 do begin
  test_add_with_resize_up_s := IntToStr(test_add_with_resize_up_i);
  test_add_with_resize_up_h := hm_set(test_add_with_resize_up_h, test_add_with_resize_up_s, test_add_with_resize_up_s);
  test_add_with_resize_up_i := test_add_with_resize_up_i + 1;
end;
  exit(hm_len(test_add_with_resize_up_h) = 5);
end;
function test_add_with_resize_down(): boolean;
var
  test_add_with_resize_down_h: HashMap;
  test_add_with_resize_down_i: integer;
  test_add_with_resize_down_s: string;
  test_add_with_resize_down_j: integer;
  test_add_with_resize_down_d: DelResult;
  test_add_with_resize_down_a: GetResult;
begin
  test_add_with_resize_down_h := make_hash_map();
  test_add_with_resize_down_i := 0;
  while test_add_with_resize_down_i < 5 do begin
  test_add_with_resize_down_s := IntToStr(test_add_with_resize_down_i);
  test_add_with_resize_down_h := hm_set(test_add_with_resize_down_h, test_add_with_resize_down_s, test_add_with_resize_down_s);
  test_add_with_resize_down_i := test_add_with_resize_down_i + 1;
end;
  test_add_with_resize_down_j := 0;
  while test_add_with_resize_down_j < 5 do begin
  test_add_with_resize_down_s := IntToStr(test_add_with_resize_down_j);
  test_add_with_resize_down_d := hm_del(test_add_with_resize_down_h, test_add_with_resize_down_s);
  test_add_with_resize_down_h := test_add_with_resize_down_d.map;
  test_add_with_resize_down_j := test_add_with_resize_down_j + 1;
end;
  test_add_with_resize_down_h := hm_set(test_add_with_resize_down_h, 'key_a', 'val_b');
  test_add_with_resize_down_a := hm_get(test_add_with_resize_down_h, 'key_a');
  exit(((hm_len(test_add_with_resize_down_h) = 1) and test_add_with_resize_down_a.found) and (test_add_with_resize_down_a.value = 'val_b'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(Ord(test_add_items()));
  writeln(Ord(test_overwrite_items()));
  writeln(Ord(test_delete_items()));
  writeln(Ord(test_access_absent_items()));
  writeln(Ord(test_add_with_resize_up()));
  writeln(Ord(test_add_with_resize_down()));
  writeln(Ord(true));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
