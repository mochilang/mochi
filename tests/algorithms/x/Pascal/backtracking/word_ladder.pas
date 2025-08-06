{$mode objfpc}
program Main;
uses SysUtils;
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
  alphabet: string;
function contains(xs: StrArray; x: string): boolean; forward;
function remove_item(xs: StrArray; x: string): StrArray; forward;
function word_ladder(current: string; path: StrArray; target: string; words: StrArray): StrArray; forward;
procedure main(); forward;
function contains(xs: StrArray; x: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = x then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function remove_item(xs: StrArray; x: string): StrArray;
var
  remove_item_res: array of string;
  remove_item_removed: boolean;
  remove_item_i: integer;
begin
  remove_item_res := [];
  remove_item_removed := false;
  remove_item_i := 0;
  while remove_item_i < Length(xs) do begin
  if not remove_item_removed and (xs[remove_item_i] = x) then begin
  remove_item_removed := true;
end else begin
  remove_item_res := concat(remove_item_res, [xs[remove_item_i]]);
end;
  remove_item_i := remove_item_i + 1;
end;
  exit(remove_item_res);
end;
function word_ladder(current: string; path: StrArray; target: string; words: StrArray): StrArray;
var
  word_ladder_i: integer;
  word_ladder_j: integer;
  word_ladder_c: string;
  word_ladder_transformed: string;
  word_ladder_new_words: StrArray;
  word_ladder_new_path: array of string;
  word_ladder_result_: array of string;
begin
  if current = target then begin
  exit(path);
end;
  word_ladder_i := 0;
  while word_ladder_i < Length(current) do begin
  word_ladder_j := 0;
  while word_ladder_j < Length(alphabet) do begin
  word_ladder_c := copy(alphabet, word_ladder_j+1, (word_ladder_j + 1 - (word_ladder_j)));
  word_ladder_transformed := (copy(current, 0+1, (word_ladder_i - (0))) + word_ladder_c) + copy(current, word_ladder_i + 1+1, (Length(current) - (word_ladder_i + 1)));
  if contains(words, word_ladder_transformed) then begin
  word_ladder_new_words := remove_item(words, word_ladder_transformed);
  word_ladder_new_path := concat(path, [word_ladder_transformed]);
  word_ladder_result_ := word_ladder(word_ladder_transformed, word_ladder_new_path, target, word_ladder_new_words);
  if Length(word_ladder_result_) > 0 then begin
  exit(word_ladder_result_);
end;
end;
  word_ladder_j := word_ladder_j + 1;
end;
  word_ladder_i := word_ladder_i + 1;
end;
  exit(StrArray([]));
end;
procedure main();
var
  main_w1: array of string;
  main_w2: array of string;
  main_w3: array of string;
  main_w4: array of string;
begin
  main_w1 := ['hot', 'dot', 'dog', 'lot', 'log', 'cog'];
  writeln(list_to_str(word_ladder('hit', ['hit'], 'cog', main_w1)));
  main_w2 := ['hot', 'dot', 'dog', 'lot', 'log'];
  writeln(list_to_str(word_ladder('hit', ['hit'], 'cog', main_w2)));
  main_w3 := ['load', 'goad', 'gold', 'lead', 'lord'];
  writeln(list_to_str(word_ladder('lead', ['lead'], 'gold', main_w3)));
  main_w4 := ['came', 'cage', 'code', 'cade', 'gave'];
  writeln(list_to_str(word_ladder('game', ['game'], 'code', main_w4)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  alphabet := 'abcdefghijklmnopqrstuvwxyz';
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
