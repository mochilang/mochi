{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type WordCount = record
  word: string;
  count: int64;
end;
type StrArray = array of string;
type WordCountArray = array of WordCount;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  freq_map: specialize TFPGMap<string, int64>;
  top_k_frequent_words_w_idx: integer;
function Map1(): specialize TFPGMap<string, real>; forward;
function makeWordCount(word: string; count: int64): WordCount; forward;
procedure heapify(heapify_arr: WordCountArray; heapify_index: int64; heapify_heap_size: int64); forward;
procedure build_max_heap(build_max_heap_arr: WordCountArray); forward;
function top_k_frequent_words(top_k_frequent_words_words: StrArray; top_k_frequent_words_k_value: int64): StrArray; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
end;
function makeWordCount(word: string; count: int64): WordCount;
begin
  Result.word := word;
  Result.count := count;
end;
procedure heapify(heapify_arr: WordCountArray; heapify_index: int64; heapify_heap_size: int64);
var
  heapify_largest: int64;
  heapify_left: int64;
  heapify_right: int64;
  heapify_left_item: WordCount;
  heapify_largest_item: WordCount;
  heapify_right_item: WordCount;
  heapify_largest_item2: WordCount;
  heapify_temp: WordCount;
begin
  heapify_largest := heapify_index;
  heapify_left := (2 * heapify_index) + 1;
  heapify_right := (2 * heapify_index) + 2;
  if heapify_left < heapify_heap_size then begin
  heapify_left_item := heapify_arr[heapify_left];
  heapify_largest_item := heapify_arr[heapify_largest];
  if heapify_left_item.count > heapify_largest_item.count then begin
  heapify_largest := heapify_left;
end;
end;
  if heapify_right < heapify_heap_size then begin
  heapify_right_item := heapify_arr[heapify_right];
  heapify_largest_item2 := heapify_arr[heapify_largest];
  if heapify_right_item.count > heapify_largest_item2.count then begin
  heapify_largest := heapify_right;
end;
end;
  if heapify_largest <> heapify_index then begin
  heapify_temp := heapify_arr[heapify_largest];
  heapify_arr[heapify_largest] := heapify_arr[heapify_index];
  heapify_arr[heapify_index] := heapify_temp;
  heapify(heapify_arr, heapify_largest, heapify_heap_size);
end;
end;
procedure build_max_heap(build_max_heap_arr: WordCountArray);
var
  build_max_heap_i: int64;
begin
  build_max_heap_i := (Length(build_max_heap_arr) div 2) - 1;
  while build_max_heap_i >= 0 do begin
  heapify(build_max_heap_arr, build_max_heap_i, Length(build_max_heap_arr));
  build_max_heap_i := build_max_heap_i - 1;
end;
end;
function top_k_frequent_words(top_k_frequent_words_words: StrArray; top_k_frequent_words_k_value: int64): StrArray;
var
  top_k_frequent_words_i: int64;
  top_k_frequent_words_w: string;
  top_k_frequent_words_heap: array of WordCount;
  top_k_frequent_words_result_: array of string;
  top_k_frequent_words_heap_size: int64;
  top_k_frequent_words_limit: int64;
  top_k_frequent_words_j: int64;
  top_k_frequent_words_item: WordCount;
begin
  freq_map := Map1();
  top_k_frequent_words_i := 0;
  while top_k_frequent_words_i < Length(top_k_frequent_words_words) do begin
  top_k_frequent_words_w := top_k_frequent_words_words[top_k_frequent_words_i];
  if freq_map.IndexOf(top_k_frequent_words_w) <> -1 then begin
  freq_map[top_k_frequent_words_w] := freq_map[top_k_frequent_words_w] + 1;
end else begin
  freq_map[top_k_frequent_words_w] := 1;
end;
  top_k_frequent_words_i := top_k_frequent_words_i + 1;
end;
  top_k_frequent_words_heap := [];
  for top_k_frequent_words_w_idx := 0 to (freq_map.Count - 1) do begin
  top_k_frequent_words_w := freq_map.Keys[top_k_frequent_words_w_idx];
  top_k_frequent_words_heap := concat(top_k_frequent_words_heap, [makeWordCount(top_k_frequent_words_w, freq_map[top_k_frequent_words_w])]);
end;
  build_max_heap(top_k_frequent_words_heap);
  top_k_frequent_words_result_ := [];
  top_k_frequent_words_heap_size := Length(top_k_frequent_words_heap);
  top_k_frequent_words_limit := top_k_frequent_words_k_value;
  if top_k_frequent_words_limit > top_k_frequent_words_heap_size then begin
  top_k_frequent_words_limit := top_k_frequent_words_heap_size;
end;
  top_k_frequent_words_j := 0;
  while top_k_frequent_words_j < top_k_frequent_words_limit do begin
  top_k_frequent_words_item := top_k_frequent_words_heap[0];
  top_k_frequent_words_result_ := concat(top_k_frequent_words_result_, StrArray([top_k_frequent_words_item.word]));
  top_k_frequent_words_heap[0] := top_k_frequent_words_heap[top_k_frequent_words_heap_size - 1];
  top_k_frequent_words_heap[top_k_frequent_words_heap_size - 1] := top_k_frequent_words_item;
  top_k_frequent_words_heap_size := top_k_frequent_words_heap_size - 1;
  heapify(top_k_frequent_words_heap, 0, top_k_frequent_words_heap_size);
  top_k_frequent_words_j := top_k_frequent_words_j + 1;
end;
  exit(top_k_frequent_words_result_);
end;
procedure main();
var
  main_sample: array of string;
begin
  main_sample := ['a', 'b', 'c', 'a', 'c', 'c'];
  writeln(list_to_str(top_k_frequent_words(main_sample, 3)));
  writeln(list_to_str(top_k_frequent_words(main_sample, 2)));
  writeln(list_to_str(top_k_frequent_words(main_sample, 1)));
  writeln(list_to_str(top_k_frequent_words(main_sample, 0)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  freq_map := specialize TFPGMap<string, int64>.Create();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
