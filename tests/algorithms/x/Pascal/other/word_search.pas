{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
type StrArrayArray = array of StrArray;
type WordSearch = record
  words: array of string;
  width: integer;
  height: integer;
  board: array of StrArray;
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
  seed: integer;
  dc: integer;
  word: string;
  rows: IntArray;
  words: StrArray;
  height: integer;
  dr: integer;
  cols: IntArray;
  width: integer;
  list_int: IntArray;
  ws: WordSearch;
  add_fake_chars: boolean;
  max: integer;
function makeWordSearch(words: StrArray; width: integer; height: integer; board: StrArrayArray): WordSearch; forward;
function rand(): integer; forward;
function rand_range(max: integer): integer; forward;
function shuffle(list_int: IntArray): IntArray; forward;
function rand_letter(): string; forward;
function make_word_search(words: StrArray; width: integer; height: integer): WordSearch; forward;
function insert_dir(ws: WordSearch; word: string; dr: integer; dc: integer; rows: IntArray; cols: IntArray): boolean; forward;
procedure generate_board(ws: WordSearch); forward;
function visualise(ws: WordSearch; add_fake_chars: boolean): string; forward;
procedure main(); forward;
function makeWordSearch(words: StrArray; width: integer; height: integer; board: StrArrayArray): WordSearch;
begin
  Result.words := words;
  Result.width := width;
  Result.height := height;
  Result.board := board;
end;
function rand(): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed);
end;
function rand_range(max: integer): integer;
begin
  exit(rand() mod max);
end;
function shuffle(list_int: IntArray): IntArray;
var
  shuffle_i: integer;
  shuffle_j: integer;
  shuffle_tmp: integer;
begin
  shuffle_i := Length(list_int) - 1;
  while shuffle_i > 0 do begin
  shuffle_j := rand_range(shuffle_i + 1);
  shuffle_tmp := list_int[shuffle_i];
  list_int[shuffle_i] := list_int[shuffle_j];
  list_int[shuffle_j] := shuffle_tmp;
  shuffle_i := shuffle_i - 1;
end;
  exit(list_int);
end;
function rand_letter(): string;
var
  rand_letter_letters: string;
  rand_letter_i: integer;
begin
  rand_letter_letters := 'abcdefghijklmnopqrstuvwxyz';
  rand_letter_i := rand_range(26);
  exit(copy(rand_letter_letters, rand_letter_i+1, (rand_letter_i + 1 - (rand_letter_i))));
end;
function make_word_search(words: StrArray; width: integer; height: integer): WordSearch;
var
  make_word_search_board: array of StrArray;
  make_word_search_r: integer;
  make_word_search_row: array of string;
  make_word_search_c: integer;
begin
  make_word_search_board := [];
  make_word_search_r := 0;
  while make_word_search_r < height do begin
  make_word_search_row := [];
  make_word_search_c := 0;
  while make_word_search_c < width do begin
  make_word_search_row := concat(make_word_search_row, StrArray(['']));
  make_word_search_c := make_word_search_c + 1;
end;
  make_word_search_board := concat(make_word_search_board, [make_word_search_row]);
  make_word_search_r := make_word_search_r + 1;
end;
  exit(makeWordSearch(words, width, height, make_word_search_board));
end;
function insert_dir(ws: WordSearch; word: string; dr: integer; dc: integer; rows: IntArray; cols: IntArray): boolean;
var
  insert_dir_word_len: integer;
  insert_dir_ri: integer;
  insert_dir_row: integer;
  insert_dir_ci: integer;
  insert_dir_col: integer;
  insert_dir_end_r: integer;
  insert_dir_end_c: integer;
  insert_dir_k: integer;
  insert_dir_ok: boolean;
  insert_dir_rr: integer;
  insert_dir_cc: integer;
  insert_dir_rr2: integer;
  insert_dir_cc2: integer;
  insert_dir_row_list: array of string;
begin
  insert_dir_word_len := Length(word);
  insert_dir_ri := 0;
  while insert_dir_ri < Length(rows) do begin
  insert_dir_row := rows[insert_dir_ri];
  insert_dir_ci := 0;
  while insert_dir_ci < Length(cols) do begin
  insert_dir_col := cols[insert_dir_ci];
  insert_dir_end_r := insert_dir_row + (dr * (insert_dir_word_len - 1));
  insert_dir_end_c := insert_dir_col + (dc * (insert_dir_word_len - 1));
  if (((insert_dir_end_r < 0) or (insert_dir_end_r >= ws.height)) or (insert_dir_end_c < 0)) or (insert_dir_end_c >= ws.width) then begin
  insert_dir_ci := insert_dir_ci + 1;
  continue;
end;
  insert_dir_k := 0;
  insert_dir_ok := true;
  while insert_dir_k < insert_dir_word_len do begin
  insert_dir_rr := insert_dir_row + (dr * insert_dir_k);
  insert_dir_cc := insert_dir_col + (dc * insert_dir_k);
  if ws.board[insert_dir_rr][insert_dir_cc] <> '' then begin
  insert_dir_ok := false;
  break;
end;
  insert_dir_k := insert_dir_k + 1;
end;
  if insert_dir_ok then begin
  insert_dir_k := 0;
  while insert_dir_k < insert_dir_word_len do begin
  insert_dir_rr2 := insert_dir_row + (dr * insert_dir_k);
  insert_dir_cc2 := insert_dir_col + (dc * insert_dir_k);
  insert_dir_row_list := ws.board[insert_dir_rr2];
  insert_dir_row_list[insert_dir_cc2] := copy(word, insert_dir_k+1, (insert_dir_k + 1 - (insert_dir_k)));
  insert_dir_k := insert_dir_k + 1;
end;
  exit(true);
end;
  insert_dir_ci := insert_dir_ci + 1;
end;
  insert_dir_ri := insert_dir_ri + 1;
end;
  exit(false);
end;
procedure generate_board(ws: WordSearch);
var
  generate_board_dirs_r: array of integer;
  generate_board_dirs_c: array of integer;
  generate_board_i: integer;
  generate_board_word: string;
  generate_board_rows: array of integer;
  generate_board_r: integer;
  generate_board_cols: array of integer;
  generate_board_c: integer;
  generate_board_d: integer;
begin
  generate_board_dirs_r := [-1, -1, 0, 1, 1, 1, 0, -1];
  generate_board_dirs_c := [0, 1, 1, 1, 0, -1, -1, -1];
  generate_board_i := 0;
  while generate_board_i < Length(ws.words) do begin
  generate_board_word := ws.words[generate_board_i];
  generate_board_rows := [];
  generate_board_r := 0;
  while generate_board_r < ws.height do begin
  generate_board_rows := concat(generate_board_rows, IntArray([generate_board_r]));
  generate_board_r := generate_board_r + 1;
end;
  generate_board_cols := [];
  generate_board_c := 0;
  while generate_board_c < ws.width do begin
  generate_board_cols := concat(generate_board_cols, IntArray([generate_board_c]));
  generate_board_c := generate_board_c + 1;
end;
  generate_board_rows := shuffle(generate_board_rows);
  generate_board_cols := shuffle(generate_board_cols);
  generate_board_d := rand_range(8);
  insert_dir(ws, generate_board_word, generate_board_dirs_r[generate_board_d], generate_board_dirs_c[generate_board_d], generate_board_rows, generate_board_cols);
  generate_board_i := generate_board_i + 1;
end;
end;
function visualise(ws: WordSearch; add_fake_chars: boolean): string;
var
  visualise_result_: string;
  visualise_r: integer;
  visualise_c: integer;
  visualise_ch: string;
begin
  visualise_result_ := '';
  visualise_r := 0;
  while visualise_r < ws.height do begin
  visualise_c := 0;
  while visualise_c < ws.width do begin
  visualise_ch := ws.board[visualise_r][visualise_c];
  if visualise_ch = '' then begin
  if add_fake_chars then begin
  visualise_ch := rand_letter();
end else begin
  visualise_ch := '#';
end;
end;
  visualise_result_ := (visualise_result_ + visualise_ch) + ' ';
  visualise_c := visualise_c + 1;
end;
  visualise_result_ := visualise_result_ + '' + #10 + '';
  visualise_r := visualise_r + 1;
end;
  exit(visualise_result_);
end;
procedure main();
var
  main_words: array of string;
  main_ws: WordSearch;
begin
  main_words := ['cat', 'dog', 'snake', 'fish'];
  main_ws := make_word_search(main_words, 10, 10);
  generate_board(main_ws);
  writeln(visualise(main_ws, true));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 123456789;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
