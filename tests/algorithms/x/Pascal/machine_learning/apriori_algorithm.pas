{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Itemset = record
  items: array of string;
  support: integer;
end;
type StrArray = array of string;
type IntArray = array of integer;
type ItemsetArray = array of Itemset;
type StrArrayArray = array of StrArray;
type StrArrayArrayArray = array of StrArrayArray;
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
  frequent_itemsets: ItemsetArray;
  fi: Itemset;
  a: StrArray;
  itemset_var: StrArrayArray;
  candidate: StrArray;
  start: integer;
  length_: integer;
  xs: StrArray;
  data: StrArrayArray;
  s: string;
  transaction: StrArray;
  k: integer;
  b: StrArray;
  min_support: integer;
  item: StrArray;
  candidates: StrArrayArrayArray;
function makeItemset(items: StrArray; support: integer): Itemset; forward;
function load_data(): StrArrayArray; forward;
function contains_string(xs: StrArray; s: string): boolean; forward;
function is_subset(candidate: StrArray; transaction: StrArray): boolean; forward;
function lists_equal(a: StrArray; b: StrArray): boolean; forward;
function contains_list(contains_list_itemset_var: StrArrayArray; item: StrArray): boolean; forward;
function count_list(contains_list_itemset_var: StrArrayArray; item: StrArray): integer; forward;
function slice_list(xs: StrArrayArray; start: integer): StrArrayArray; forward;
function combinations_lists(xs: StrArrayArray; k: integer): StrArrayArrayArray; forward;
function prune(contains_list_itemset_var: StrArrayArray; candidates: StrArrayArrayArray; prune_length_: integer): StrArrayArray; forward;
function sort_strings(xs: StrArray): StrArray; forward;
function itemset_to_string(xs: StrArray): string; forward;
function apriori(data: StrArrayArray; min_support: integer): ItemsetArray; forward;
function makeItemset(items: StrArray; support: integer): Itemset;
begin
  Result.items := items;
  Result.support := support;
end;
function load_data(): StrArrayArray;
begin
  exit([['milk'], ['milk', 'butter'], ['milk', 'bread'], ['milk', 'bread', 'chips']]);
end;
function contains_string(xs: StrArray; s: string): boolean;
var
  contains_string_v: string;
begin
  for contains_string_v in xs do begin
  if contains_string_v = s then begin
  exit(true);
end;
end;
  exit(false);
end;
function is_subset(candidate: StrArray; transaction: StrArray): boolean;
var
  is_subset_it: string;
begin
  for is_subset_it in candidate do begin
  if not contains_string(transaction, is_subset_it) then begin
  exit(false);
end;
end;
  exit(true);
end;
function lists_equal(a: StrArray; b: StrArray): boolean;
var
  lists_equal_i: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  lists_equal_i := 0;
  while lists_equal_i < Length(a) do begin
  if a[lists_equal_i] <> b[lists_equal_i] then begin
  exit(false);
end;
  lists_equal_i := lists_equal_i + 1;
end;
  exit(true);
end;
function contains_list(contains_list_itemset_var: StrArrayArray; item: StrArray): boolean;
var
  contains_list_l: StrArray;
begin
  for contains_list_l in itemset_var do begin
  if lists_equal(contains_list_l, item) then begin
  exit(true);
end;
end;
  exit(false);
end;
function count_list(contains_list_itemset_var: StrArrayArray; item: StrArray): integer;
var
  count_list_c: integer;
  count_list_l: int64;
begin
  count_list_c := 0;
  for count_list_l in itemset_var do begin
  if lists_equal(count_list_l, item) then begin
  count_list_c := count_list_c + 1;
end;
end;
  exit(count_list_c);
end;
function slice_list(xs: StrArrayArray; start: integer): StrArrayArray;
var
  slice_list_res: array of StrArray;
  slice_list_i: integer;
begin
  slice_list_res := [];
  slice_list_i := start;
  while slice_list_i < Length(xs) do begin
  slice_list_res := concat(slice_list_res, [xs[slice_list_i]]);
  slice_list_i := slice_list_i + 1;
end;
  exit(slice_list_res);
end;
function combinations_lists(xs: StrArrayArray; k: integer): StrArrayArrayArray;
var
  combinations_lists_result_: array of StrArrayArray;
  combinations_lists_i: integer;
  combinations_lists_head: array of string;
  combinations_lists_tail: StrArrayArray;
  combinations_lists_tail_combos: array of StrArrayArray;
  combinations_lists_combo: StrArrayArray;
  combinations_lists_new_combo: array of StrArray;
  combinations_lists_c: StrArray;
begin
  combinations_lists_result_ := [];
  if k = 0 then begin
  combinations_lists_result_ := concat(combinations_lists_result_, [[]]);
  exit(combinations_lists_result_);
end;
  combinations_lists_i := 0;
  while combinations_lists_i < Length(xs) do begin
  combinations_lists_head := xs[combinations_lists_i];
  combinations_lists_tail := slice_list(xs, combinations_lists_i + 1);
  combinations_lists_tail_combos := combinations_lists(combinations_lists_tail, k - 1);
  for combinations_lists_combo in combinations_lists_tail_combos do begin
  combinations_lists_new_combo := [];
  combinations_lists_new_combo := concat(combinations_lists_new_combo, [combinations_lists_head]);
  for combinations_lists_c in combinations_lists_combo do begin
  combinations_lists_new_combo := concat(combinations_lists_new_combo, [combinations_lists_c]);
end;
  combinations_lists_result_ := concat(combinations_lists_result_, [combinations_lists_new_combo]);
end;
  combinations_lists_i := combinations_lists_i + 1;
end;
  exit(combinations_lists_result_);
end;
function prune(contains_list_itemset_var: StrArrayArray; candidates: StrArrayArrayArray; prune_length_: integer): StrArrayArray;
var
  prune_pruned: array of StrArray;
  prune_candidate: StrArrayArray;
  prune_is_subsequence: boolean;
  prune_item: StrArray;
  prune_merged: array of string;
  prune_s: string;
begin
  prune_pruned := [];
  for prune_candidate in candidates do begin
  prune_is_subsequence := true;
  for prune_item in prune_candidate do begin
  if not contains_list(itemset_var, prune_item) or (count_list(itemset_var, prune_item) < (length_ - 1)) then begin
  prune_is_subsequence := false;
  break;
end;
end;
  if prune_is_subsequence then begin
  prune_merged := [];
  for prune_item in prune_candidate do begin
  for prune_s in prune_item do begin
  if not contains_string(prune_merged, prune_s) then begin
  prune_merged := concat(prune_merged, StrArray([prune_s]));
end;
end;
end;
  prune_pruned := concat(prune_pruned, [prune_merged]);
end;
end;
  exit(prune_pruned);
end;
function sort_strings(xs: StrArray): StrArray;
var
  sort_strings_res: array of string;
  sort_strings_s: string;
  sort_strings_i: integer;
  sort_strings_j: integer;
  sort_strings_tmp: string;
begin
  sort_strings_res := [];
  for sort_strings_s in xs do begin
  sort_strings_res := concat(sort_strings_res, StrArray([sort_strings_s]));
end;
  sort_strings_i := 0;
  while sort_strings_i < Length(sort_strings_res) do begin
  sort_strings_j := sort_strings_i + 1;
  while sort_strings_j < Length(sort_strings_res) do begin
  if sort_strings_res[sort_strings_j] < sort_strings_res[sort_strings_i] then begin
  sort_strings_tmp := sort_strings_res[sort_strings_i];
  sort_strings_res[sort_strings_i] := sort_strings_res[sort_strings_j];
  sort_strings_res[sort_strings_j] := sort_strings_tmp;
end;
  sort_strings_j := sort_strings_j + 1;
end;
  sort_strings_i := sort_strings_i + 1;
end;
  exit(sort_strings_res);
end;
function itemset_to_string(xs: StrArray): string;
var
  itemset_to_string_s: string;
  itemset_to_string_i: integer;
begin
  itemset_to_string_s := '[';
  itemset_to_string_i := 0;
  while itemset_to_string_i < Length(xs) do begin
  if itemset_to_string_i > 0 then begin
  itemset_to_string_s := itemset_to_string_s + ', ';
end;
  itemset_to_string_s := ((itemset_to_string_s + '''') + xs[itemset_to_string_i]) + '''';
  itemset_to_string_i := itemset_to_string_i + 1;
end;
  itemset_to_string_s := itemset_to_string_s + ']';
  exit(itemset_to_string_s);
end;
function apriori(data: StrArrayArray; min_support: integer): ItemsetArray;
var
  apriori_itemset_var: array of StrArray;
  apriori_transaction: StrArray;
  apriori_t: array of string;
  apriori_v: string;
  apriori_frequent: array of Itemset;
  apriori_length_: integer;
  apriori_counts: array of integer;
  apriori_idx: integer;
  apriori_j: integer;
  apriori_candidate: array of string;
  apriori_new_itemset: array of StrArray;
  apriori_k: integer;
  apriori_m: integer;
  apriori_sorted_item: StrArray;
  apriori_combos: StrArrayArrayArray;
begin
  apriori_itemset_var := [];
  for apriori_transaction in data do begin
  apriori_t := [];
  for apriori_v in apriori_transaction do begin
  apriori_t := concat(apriori_t, StrArray([apriori_v]));
end;
  apriori_itemset_var := concat(apriori_itemset_var, [apriori_t]);
end;
  apriori_frequent := [];
  apriori_length_ := 1;
  while Length(apriori_itemset_var) > 0 do begin
  apriori_counts := [];
  apriori_idx := 0;
  while apriori_idx < Length(apriori_itemset_var) do begin
  apriori_counts := concat(apriori_counts, IntArray([0]));
  apriori_idx := apriori_idx + 1;
end;
  for apriori_transaction in data do begin
  apriori_j := 0;
  while apriori_j < Length(apriori_itemset_var) do begin
  apriori_candidate := apriori_itemset_var[apriori_j];
  if is_subset(apriori_candidate, apriori_transaction) then begin
  apriori_counts[apriori_j] := apriori_counts[apriori_j] + 1;
end;
  apriori_j := apriori_j + 1;
end;
end;
  apriori_new_itemset := [];
  apriori_k := 0;
  while apriori_k < Length(apriori_itemset_var) do begin
  if apriori_counts[apriori_k] >= min_support then begin
  apriori_new_itemset := concat(apriori_new_itemset, [apriori_itemset_var[apriori_k]]);
end;
  apriori_k := apriori_k + 1;
end;
  apriori_itemset_var := apriori_new_itemset;
  apriori_m := 0;
  while apriori_m < Length(apriori_itemset_var) do begin
  apriori_sorted_item := sort_strings(apriori_itemset_var[apriori_m]);
  apriori_frequent := concat(apriori_frequent, [makeItemset(apriori_sorted_item, apriori_counts[apriori_m])]);
  apriori_m := apriori_m + 1;
end;
  apriori_length_ := apriori_length_ + 1;
  apriori_combos := combinations_lists(apriori_itemset_var, apriori_length_);
  apriori_itemset_var := prune(apriori_itemset_var, apriori_combos, apriori_length_);
end;
  exit(apriori_frequent);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  frequent_itemsets := apriori(load_data(), 2);
  for fi in frequent_itemsets do begin
  writeln((itemset_to_string(fi.items) + ': ') + IntToStr(fi.support));
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
