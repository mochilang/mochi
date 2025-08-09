{$mode objfpc}
program Main;
uses SysUtils, fgl;
type StrArray = array of string;
type BoolArray = array of boolean;
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
  s: string;
  b: boolean;
  words: StrArray;
function build_set(words: StrArray): specialize TFPGMap<string, boolean>; forward;
function word_break(s: string; words: StrArray): boolean; forward;
procedure print_bool(b: boolean); forward;
function build_set(words: StrArray): specialize TFPGMap<string, boolean>;
var
  build_set_m: specialize TFPGMap<string, boolean>;
  build_set_w: string;
begin
  build_set_m := specialize TFPGMap<string, boolean>.Create();
  for build_set_w in words do begin
  build_set_m[build_set_w] := true;
end;
  exit(build_set_m);
end;
function word_break(s: string; words: StrArray): boolean;
var
  word_break_n: integer;
  word_break_dict: specialize TFPGMap<string, boolean>;
  word_break_dp: array of boolean;
  word_break_i: integer;
  word_break_j: integer;
  word_break_sub: string;
begin
  word_break_n := Length(s);
  word_break_dict := build_set(words);
  word_break_dp := [];
  word_break_i := 0;
  while word_break_i <= word_break_n do begin
  word_break_dp := concat(word_break_dp, [false]);
  word_break_i := word_break_i + 1;
end;
  word_break_dp[0] := true;
  word_break_i := 1;
  while word_break_i <= word_break_n do begin
  word_break_j := 0;
  while word_break_j < word_break_i do begin
  if word_break_dp[word_break_j] then begin
  word_break_sub := copy(s, word_break_j+1, (word_break_i - (word_break_j)));
  if word_break_dict.IndexOf(word_break_sub) <> -1 then begin
  word_break_dp[word_break_i] := true;
  word_break_j := word_break_i;
end;
end;
  word_break_j := word_break_j + 1;
end;
  word_break_i := word_break_i + 1;
end;
  exit(word_break_dp[word_break_n]);
end;
procedure print_bool(b: boolean);
begin
  if b then begin
  writeln(Ord(true));
end else begin
  writeln(Ord(false));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  print_bool(word_break('applepenapple', ['apple', 'pen']));
  print_bool(word_break('catsandog', ['cats', 'dog', 'sand', 'and', 'cat']));
  print_bool(word_break('cars', ['car', 'ca', 'rs']));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
