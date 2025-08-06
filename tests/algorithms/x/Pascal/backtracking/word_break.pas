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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function contains(words: StrArray; target: string): boolean; forward;
function backtrack(s: string; word_dict: StrArray; start: integer): boolean; forward;
function word_break(s: string; word_dict: StrArray): boolean; forward;
function contains(words: StrArray; target: string): boolean;
var
  contains_w: string;
begin
  for contains_w in words do begin
  if contains_w = target then begin
  exit(true);
end;
end;
  exit(false);
end;
function backtrack(s: string; word_dict: StrArray; start: integer): boolean;
var
  backtrack_end: integer;
  backtrack_substr: string;
begin
  if start = Length(s) then begin
  exit(true);
end;
  backtrack_end := start + 1;
  while backtrack_end <= Length(s) do begin
  backtrack_substr := copy(s, start+1, (backtrack_end - (start)));
  if contains(word_dict, backtrack_substr) and backtrack(s, word_dict, backtrack_end) then begin
  exit(true);
end;
  backtrack_end := backtrack_end + 1;
end;
  exit(false);
end;
function word_break(s: string; word_dict: StrArray): boolean;
begin
  exit(backtrack(s, word_dict, 0));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(word_break('leetcode', ['leet', 'code']), true)));
  writeln(LowerCase(BoolToStr(word_break('applepenapple', ['apple', 'pen']), true)));
  writeln(LowerCase(BoolToStr(word_break('catsandog', ['cats', 'dog', 'sand', 'and', 'cat']), true)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
