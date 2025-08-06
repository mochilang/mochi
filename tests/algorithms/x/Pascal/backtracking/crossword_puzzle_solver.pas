{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type BoolArray = array of boolean;
type StrArrayArray = array of StrArray;
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
  puzzle: array of StrArray;
  words: array of string;
  used: array of boolean;
  row: StrArray;
function is_valid(puzzle: StrArrayArray; word: string; row: integer; col: integer; vertical: boolean): boolean; forward;
procedure place_word(puzzle: StrArrayArray; word: string; row: integer; col: integer; vertical: boolean); forward;
procedure remove_word(puzzle: StrArrayArray; word: string; row: integer; col: integer; vertical: boolean); forward;
function solve_crossword(puzzle: StrArrayArray; words: StrArray; used: BoolArray): boolean; forward;
function is_valid(puzzle: StrArrayArray; word: string; row: integer; col: integer; vertical: boolean): boolean;
var
  is_valid_i: integer;
begin
  for is_valid_i := 0 to (Length(word) - 1) do begin
  if vertical then begin
  if ((row + is_valid_i) >= Length(puzzle)) or (puzzle[row + is_valid_i][col] <> '') then begin
  exit(false);
end;
end else begin
  if ((col + is_valid_i) >= Length(puzzle[0])) or (puzzle[row][col + is_valid_i] <> '') then begin
  exit(false);
end;
end;
end;
  exit(true);
end;
procedure place_word(puzzle: StrArrayArray; word: string; row: integer; col: integer; vertical: boolean);
var
  place_word_i: integer;
  place_word_ch: string;
begin
  for place_word_i := 0 to (Length(word) - 1) do begin
  place_word_ch := word[place_word_i+1];
  if vertical then begin
  puzzle[row + place_word_i][col] := place_word_ch;
end else begin
  puzzle[row][col + place_word_i] := place_word_ch;
end;
end;
end;
procedure remove_word(puzzle: StrArrayArray; word: string; row: integer; col: integer; vertical: boolean);
var
  remove_word_i: integer;
begin
  for remove_word_i := 0 to (Length(word) - 1) do begin
  if vertical then begin
  puzzle[row + remove_word_i][col] := '';
end else begin
  puzzle[row][col + remove_word_i] := '';
end;
end;
end;
function solve_crossword(puzzle: StrArrayArray; words: StrArray; used: BoolArray): boolean;
var
  solve_crossword_row: integer;
  solve_crossword_col: integer;
  solve_crossword_i: integer;
  solve_crossword_word: string;
  solve_crossword_vertical: boolean;
begin
  for solve_crossword_row := 0 to (Length(puzzle) - 1) do begin
  for solve_crossword_col := 0 to (Length(puzzle[0]) - 1) do begin
  if puzzle[solve_crossword_row][solve_crossword_col] = '' then begin
  for solve_crossword_i := 0 to (Length(words) - 1) do begin
  if not used[solve_crossword_i] then begin
  solve_crossword_word := words[solve_crossword_i];
  for solve_crossword_vertical in [true, false] do begin
  if is_valid(puzzle, solve_crossword_word, solve_crossword_row, solve_crossword_col, solve_crossword_vertical) then begin
  place_word(puzzle, solve_crossword_word, solve_crossword_row, solve_crossword_col, solve_crossword_vertical);
  used[solve_crossword_i] := true;
  if solve_crossword(puzzle, words, used) then begin
  exit(true);
end;
  used[solve_crossword_i] := false;
  remove_word(puzzle, solve_crossword_word, solve_crossword_row, solve_crossword_col, solve_crossword_vertical);
end;
end;
end;
end;
  exit(false);
end;
end;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  puzzle := [['', '', ''], ['', '', ''], ['', '', '']];
  words := ['cat', 'dog', 'car'];
  used := [false, false, false];
  if solve_crossword(puzzle, words, used) then begin
  writeln('Solution found:');
  for row in puzzle do begin
  writeln(list_to_str(row));
end;
end else begin
  writeln('No solution found:');
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
