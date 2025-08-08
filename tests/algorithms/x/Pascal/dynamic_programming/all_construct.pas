{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
  wordBank: StrArray;
  target: string;
function allConstruct(target: string; wordBank: StrArray): StrArrayArray; forward;
function allConstruct(target: string; wordBank: StrArray): StrArrayArray;
var
  allConstruct_tableSize: integer;
  allConstruct_table: array of StrArrayArray;
  allConstruct_idx: integer;
  allConstruct_empty: array of StrArray;
  allConstruct_base: array of string;
  allConstruct_i: integer;
  allConstruct_w: integer;
  allConstruct_word: string;
  allConstruct_wordLen: integer;
  allConstruct_k: integer;
  allConstruct_way: array of string;
  allConstruct_combination: array of string;
  allConstruct_m: integer;
  allConstruct_nextIndex: integer;
begin
  allConstruct_tableSize := Length(target) + 1;
  allConstruct_table := [];
  allConstruct_idx := 0;
  while allConstruct_idx < allConstruct_tableSize do begin
  allConstruct_empty := [];
  allConstruct_table := concat(allConstruct_table, [allConstruct_empty]);
  allConstruct_idx := allConstruct_idx + 1;
end;
  allConstruct_base := [];
  allConstruct_table[0] := [allConstruct_base];
  allConstruct_i := 0;
  while allConstruct_i < allConstruct_tableSize do begin
  if Length(allConstruct_table[allConstruct_i]) <> 0 then begin
  allConstruct_w := 0;
  while allConstruct_w < Length(wordBank) do begin
  allConstruct_word := wordBank[allConstruct_w];
  allConstruct_wordLen := Length(allConstruct_word);
  if copy(target, allConstruct_i+1, (allConstruct_i + allConstruct_wordLen - (allConstruct_i))) = allConstruct_word then begin
  allConstruct_k := 0;
  while allConstruct_k < Length(allConstruct_table[allConstruct_i]) do begin
  allConstruct_way := allConstruct_table[allConstruct_i][allConstruct_k];
  allConstruct_combination := [];
  allConstruct_m := 0;
  while allConstruct_m < Length(allConstruct_way) do begin
  allConstruct_combination := concat(allConstruct_combination, StrArray([allConstruct_way[allConstruct_m]]));
  allConstruct_m := allConstruct_m + 1;
end;
  allConstruct_combination := concat(allConstruct_combination, StrArray([allConstruct_word]));
  allConstruct_nextIndex := allConstruct_i + allConstruct_wordLen;
  allConstruct_table[allConstruct_nextIndex] := concat(allConstruct_table[allConstruct_nextIndex], [allConstruct_combination]);
  allConstruct_k := allConstruct_k + 1;
end;
end;
  allConstruct_w := allConstruct_w + 1;
end;
end;
  allConstruct_i := allConstruct_i + 1;
end;
  exit(allConstruct_table[Length(target)]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(allConstruct('jwajalapa', ['jwa', 'j', 'w', 'a', 'la', 'lapa'])));
  writeln(list_int_to_str(allConstruct('rajamati', ['s', 'raj', 'amat', 'raja', 'ma', 'i', 't'])));
  writeln(list_int_to_str(allConstruct('hexagonosaurus', ['h', 'ex', 'hex', 'ag', 'ago', 'ru', 'auru', 'rus', 'go', 'no', 'o', 's'])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
