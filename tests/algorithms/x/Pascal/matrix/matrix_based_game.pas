{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Coord = record
  x: integer;
  y: integer;
end;
type CoordArray = array of Coord;
type StrArray = array of string;
type IntArray = array of integer;
type StrArrayArray = array of StrArray;
type PlayResult = record
  matrix: array of StrArray;
  score: integer;
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
function makePlayResult(matrix: StrArrayArray; score: integer): PlayResult; forward;
function makeCoord(x: integer; y: integer): Coord; forward;
function is_alnum(is_alnum_ch: string): boolean; forward;
function to_int(to_int_token: string): integer; forward;
function split(split_s: string; split_sep: string): StrArray; forward;
function parse_moves(parse_moves_input_str: string): CoordArray; forward;
procedure validate_matrix_size(validate_matrix_size_size: integer); forward;
procedure validate_matrix_content(validate_matrix_content_matrix: StrArray; validate_matrix_content_size: integer); forward;
procedure validate_moves(validate_moves_moves: CoordArray; validate_moves_size: integer); forward;
function contains(contains_pos: CoordArray; contains_r: integer; contains_c: integer): boolean; forward;
function find_repeat(find_repeat_matrix_g: StrArrayArray; find_repeat_row: integer; find_repeat_column: integer; find_repeat_size: integer): CoordArray; forward;
function increment_score(increment_score_count: integer): integer; forward;
function move_x(move_x_matrix_g: StrArrayArray; move_x_column: integer; move_x_size: integer): StrArrayArray; forward;
function move_y(move_y_matrix_g: StrArrayArray; move_y_size: integer): StrArrayArray; forward;
function play(play_matrix_g: StrArrayArray; play_pos_x: integer; play_pos_y: integer; play_size: integer): PlayResult; forward;
function build_matrix(build_matrix_matrix: StrArray): StrArrayArray; forward;
function process_game(process_game_size: integer; process_game_matrix: StrArray; process_game_moves: CoordArray): integer; forward;
procedure main(); forward;
function makePlayResult(matrix: StrArrayArray; score: integer): PlayResult;
begin
  Result.matrix := matrix;
  Result.score := score;
end;
function makeCoord(x: integer; y: integer): Coord;
begin
  Result.x := x;
  Result.y := y;
end;
function is_alnum(is_alnum_ch: string): boolean;
begin
  exit((((is_alnum_ch >= '0') and (is_alnum_ch <= '9')) or ((is_alnum_ch >= 'A') and (is_alnum_ch <= 'Z'))) or ((is_alnum_ch >= 'a') and (is_alnum_ch <= 'z')));
end;
function to_int(to_int_token: string): integer;
var
  to_int_res: integer;
  to_int_i: integer;
begin
  to_int_res := 0;
  to_int_i := 0;
  while to_int_i < Length(to_int_token) do begin
  to_int_res := (to_int_res * 10) + StrToInt(copy(to_int_token, to_int_i+1, (to_int_i + 1 - (to_int_i))));
  to_int_i := to_int_i + 1;
end;
  exit(to_int_res);
end;
function split(split_s: string; split_sep: string): StrArray;
var
  split_res: array of string;
  split_current: string;
  split_i: integer;
  split_ch: string;
begin
  split_res := [];
  split_current := '';
  split_i := 0;
  while split_i < Length(split_s) do begin
  split_ch := copy(split_s, split_i+1, (split_i + 1 - (split_i)));
  if split_ch = split_sep then begin
  split_res := concat(split_res, StrArray([split_current]));
  split_current := '';
end else begin
  split_current := split_current + split_ch;
end;
  split_i := split_i + 1;
end;
  split_res := concat(split_res, StrArray([split_current]));
  exit(split_res);
end;
function parse_moves(parse_moves_input_str: string): CoordArray;
var
  parse_moves_pairs: StrArray;
  parse_moves_moves: array of Coord;
  parse_moves_i: integer;
  parse_moves_pair: string;
  parse_moves_numbers: array of string;
  parse_moves_num: string;
  parse_moves_j: integer;
  parse_moves_ch: string;
  parse_moves_x: integer;
  parse_moves_y: integer;
begin
  parse_moves_pairs := split(parse_moves_input_str, ',');
  parse_moves_moves := [];
  parse_moves_i := 0;
  while parse_moves_i < Length(parse_moves_pairs) do begin
  parse_moves_pair := parse_moves_pairs[parse_moves_i];
  parse_moves_numbers := [];
  parse_moves_num := '';
  parse_moves_j := 0;
  while parse_moves_j < Length(parse_moves_pair) do begin
  parse_moves_ch := copy(parse_moves_pair, parse_moves_j+1, (parse_moves_j + 1 - (parse_moves_j)));
  if parse_moves_ch = ' ' then begin
  if parse_moves_num <> '' then begin
  parse_moves_numbers := concat(parse_moves_numbers, StrArray([parse_moves_num]));
  parse_moves_num := '';
end;
end else begin
  parse_moves_num := parse_moves_num + parse_moves_ch;
end;
  parse_moves_j := parse_moves_j + 1;
end;
  if parse_moves_num <> '' then begin
  parse_moves_numbers := concat(parse_moves_numbers, StrArray([parse_moves_num]));
end;
  if Length(parse_moves_numbers) <> 2 then begin
  panic('Each move must have exactly two numbers.');
end;
  parse_moves_x := to_int(parse_moves_numbers[0]);
  parse_moves_y := to_int(parse_moves_numbers[1]);
  parse_moves_moves := concat(parse_moves_moves, [makeCoord(parse_moves_x, parse_moves_y)]);
  parse_moves_i := parse_moves_i + 1;
end;
  exit(parse_moves_moves);
end;
procedure validate_matrix_size(validate_matrix_size_size: integer);
begin
  if validate_matrix_size_size <= 0 then begin
  panic('Matrix size must be a positive integer.');
end;
end;
procedure validate_matrix_content(validate_matrix_content_matrix: StrArray; validate_matrix_content_size: integer);
var
  validate_matrix_content_i: integer;
  validate_matrix_content_row: string;
  validate_matrix_content_j: integer;
  validate_matrix_content_ch: string;
begin
  if Length(validate_matrix_content_matrix) <> validate_matrix_content_size then begin
  panic('The matrix dont match with size.');
end;
  validate_matrix_content_i := 0;
  while validate_matrix_content_i < validate_matrix_content_size do begin
  validate_matrix_content_row := validate_matrix_content_matrix[validate_matrix_content_i];
  if Length(validate_matrix_content_row) <> validate_matrix_content_size then begin
  panic(('Each row in the matrix must have exactly ' + IntToStr(validate_matrix_content_size)) + ' characters.');
end;
  validate_matrix_content_j := 0;
  while validate_matrix_content_j < validate_matrix_content_size do begin
  validate_matrix_content_ch := copy(validate_matrix_content_row, validate_matrix_content_j+1, (validate_matrix_content_j + 1 - (validate_matrix_content_j)));
  if not is_alnum(validate_matrix_content_ch) then begin
  panic('Matrix rows can only contain letters and numbers.');
end;
  validate_matrix_content_j := validate_matrix_content_j + 1;
end;
  validate_matrix_content_i := validate_matrix_content_i + 1;
end;
end;
procedure validate_moves(validate_moves_moves: CoordArray; validate_moves_size: integer);
var
  validate_moves_i: integer;
  validate_moves_mv: Coord;
begin
  validate_moves_i := 0;
  while validate_moves_i < Length(validate_moves_moves) do begin
  validate_moves_mv := validate_moves_moves[validate_moves_i];
  if (((validate_moves_mv.x < 0) or (validate_moves_mv.x >= validate_moves_size)) or (validate_moves_mv.y < 0)) or (validate_moves_mv.y >= validate_moves_size) then begin
  panic('Move is out of bounds for a matrix.');
end;
  validate_moves_i := validate_moves_i + 1;
end;
end;
function contains(contains_pos: CoordArray; contains_r: integer; contains_c: integer): boolean;
var
  contains_i: integer;
  contains_p: Coord;
begin
  contains_i := 0;
  while contains_i < Length(contains_pos) do begin
  contains_p := contains_pos[contains_i];
  if (contains_p.x = contains_r) and (contains_p.y = contains_c) then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function find_repeat(find_repeat_matrix_g: StrArrayArray; find_repeat_row: integer; find_repeat_column: integer; find_repeat_size: integer): CoordArray;
var
  find_repeat_visited: array of Coord;
  find_repeat_repeated: array of Coord;
  find_repeat_color: string;
  find_repeat_stack: array of Coord;
  find_repeat_idx: integer;
  find_repeat_pos: Coord;
begin
  find_repeat_column := (find_repeat_size - 1) - find_repeat_column;
  find_repeat_visited := [];
  find_repeat_repeated := [];
  find_repeat_color := find_repeat_matrix_g[find_repeat_column][find_repeat_row];
  if find_repeat_color = '-' then begin
  exit(find_repeat_repeated);
end;
  find_repeat_stack := [makeCoord(find_repeat_column, find_repeat_row)];
  while Length(find_repeat_stack) > 0 do begin
  find_repeat_idx := Length(find_repeat_stack) - 1;
  find_repeat_pos := find_repeat_stack[find_repeat_idx];
  find_repeat_stack := copy(find_repeat_stack, 0, (find_repeat_idx - (0)));
  if (((find_repeat_pos.x < 0) or (find_repeat_pos.x >= find_repeat_size)) or (find_repeat_pos.y < 0)) or (find_repeat_pos.y >= find_repeat_size) then begin
  continue;
end;
  if contains(find_repeat_visited, find_repeat_pos.x, find_repeat_pos.y) then begin
  continue;
end;
  find_repeat_visited := concat(find_repeat_visited, [find_repeat_pos]);
  if find_repeat_matrix_g[find_repeat_pos.x][find_repeat_pos.y] = find_repeat_color then begin
  find_repeat_repeated := concat(find_repeat_repeated, [find_repeat_pos]);
  find_repeat_stack := concat(find_repeat_stack, [makeCoord(find_repeat_pos.x - 1, find_repeat_pos.y)]);
  find_repeat_stack := concat(find_repeat_stack, [makeCoord(find_repeat_pos.x + 1, find_repeat_pos.y)]);
  find_repeat_stack := concat(find_repeat_stack, [makeCoord(find_repeat_pos.x, find_repeat_pos.y - 1)]);
  find_repeat_stack := concat(find_repeat_stack, [makeCoord(find_repeat_pos.x, find_repeat_pos.y + 1)]);
end;
end;
  exit(find_repeat_repeated);
end;
function increment_score(increment_score_count: integer): integer;
begin
  exit((increment_score_count * (increment_score_count + 1)) div 2);
end;
function move_x(move_x_matrix_g: StrArrayArray; move_x_column: integer; move_x_size: integer): StrArrayArray;
var
  move_x_new_list: array of string;
  move_x_row: integer;
  move_x_val: string;
begin
  move_x_new_list := [];
  move_x_row := 0;
  while move_x_row < move_x_size do begin
  move_x_val := move_x_matrix_g[move_x_row][move_x_column];
  if move_x_val <> '-' then begin
  move_x_new_list := concat(move_x_new_list, StrArray([move_x_val]));
end else begin
  move_x_new_list := concat([move_x_val], move_x_new_list);
end;
  move_x_row := move_x_row + 1;
end;
  move_x_row := 0;
  while move_x_row < move_x_size do begin
  move_x_matrix_g[move_x_row][move_x_column] := move_x_new_list[move_x_row];
  move_x_row := move_x_row + 1;
end;
  exit(move_x_matrix_g);
end;
function move_y(move_y_matrix_g: StrArrayArray; move_y_size: integer): StrArrayArray;
var
  move_y_empty_cols: array of integer;
  move_y_column: integer;
  move_y_row: integer;
  move_y_all_empty: boolean;
  move_y_i: integer;
  move_y_col: integer;
  move_y_c: integer;
  move_y_r: integer;
begin
  move_y_empty_cols := [];
  move_y_column := move_y_size - 1;
  while move_y_column >= 0 do begin
  move_y_row := 0;
  move_y_all_empty := true;
  while move_y_row < move_y_size do begin
  if move_y_matrix_g[move_y_row][move_y_column] <> '-' then begin
  move_y_all_empty := false;
  break;
end;
  move_y_row := move_y_row + 1;
end;
  if move_y_all_empty then begin
  move_y_empty_cols := concat(move_y_empty_cols, IntArray([move_y_column]));
end;
  move_y_column := move_y_column - 1;
end;
  move_y_i := 0;
  while move_y_i < Length(move_y_empty_cols) do begin
  move_y_col := move_y_empty_cols[move_y_i];
  move_y_c := move_y_col + 1;
  while move_y_c < move_y_size do begin
  move_y_r := 0;
  while move_y_r < move_y_size do begin
  move_y_matrix_g[move_y_r][move_y_c - 1] := move_y_matrix_g[move_y_r][move_y_c];
  move_y_r := move_y_r + 1;
end;
  move_y_c := move_y_c + 1;
end;
  move_y_r := 0;
  while move_y_r < move_y_size do begin
  move_y_matrix_g[move_y_r][move_y_size - 1] := '-';
  move_y_r := move_y_r + 1;
end;
  move_y_i := move_y_i + 1;
end;
  exit(move_y_matrix_g);
end;
function play(play_matrix_g: StrArrayArray; play_pos_x: integer; play_pos_y: integer; play_size: integer): PlayResult;
var
  play_same_colors: CoordArray;
  play_i: integer;
  play_p: Coord;
  play_column: integer;
  play_sc: integer;
begin
  play_same_colors := find_repeat(play_matrix_g, play_pos_x, play_pos_y, play_size);
  if Length(play_same_colors) <> 0 then begin
  play_i := 0;
  while play_i < Length(play_same_colors) do begin
  play_p := play_same_colors[play_i];
  play_matrix_g[play_p.x][play_p.y] := '-';
  play_i := play_i + 1;
end;
  play_column := 0;
  while play_column < play_size do begin
  play_matrix_g := move_x(play_matrix_g, play_column, play_size);
  play_column := play_column + 1;
end;
  play_matrix_g := move_y(play_matrix_g, play_size);
end;
  play_sc := increment_score(Length(play_same_colors));
  exit(makePlayResult(play_matrix_g, play_sc));
end;
function build_matrix(build_matrix_matrix: StrArray): StrArrayArray;
var
  build_matrix_res: array of StrArray;
  build_matrix_i: integer;
  build_matrix_row: string;
  build_matrix_row_list: array of string;
  build_matrix_j: integer;
begin
  build_matrix_res := [];
  build_matrix_i := 0;
  while build_matrix_i < Length(build_matrix_matrix) do begin
  build_matrix_row := build_matrix_matrix[build_matrix_i];
  build_matrix_row_list := [];
  build_matrix_j := 0;
  while build_matrix_j < Length(build_matrix_row) do begin
  build_matrix_row_list := concat(build_matrix_row_list, StrArray([copy(build_matrix_row, build_matrix_j+1, (build_matrix_j + 1 - (build_matrix_j)))]));
  build_matrix_j := build_matrix_j + 1;
end;
  build_matrix_res := concat(build_matrix_res, [build_matrix_row_list]);
  build_matrix_i := build_matrix_i + 1;
end;
  exit(build_matrix_res);
end;
function process_game(process_game_size: integer; process_game_matrix: StrArray; process_game_moves: CoordArray): integer;
var
  process_game_game_matrix: StrArrayArray;
  process_game_total: integer;
  process_game_i: integer;
  process_game_mv: Coord;
  process_game_res: PlayResult;
begin
  process_game_game_matrix := build_matrix(process_game_matrix);
  process_game_total := 0;
  process_game_i := 0;
  while process_game_i < Length(process_game_moves) do begin
  process_game_mv := process_game_moves[process_game_i];
  process_game_res := play(process_game_game_matrix, process_game_mv.x, process_game_mv.y, process_game_size);
  process_game_game_matrix := process_game_res.matrix;
  process_game_total := process_game_total + process_game_res.score;
  process_game_i := process_game_i + 1;
end;
  exit(process_game_total);
end;
procedure main();
var
  main_size: integer;
  main_matrix: array of string;
  main_moves: CoordArray;
  main_score: integer;
begin
  main_size := 4;
  main_matrix := ['RRBG', 'RBBG', 'YYGG', 'XYGG'];
  main_moves := parse_moves('0 1,1 1');
  validate_matrix_size(main_size);
  validate_matrix_content(main_matrix, main_size);
  validate_moves(main_moves, main_size);
  main_score := process_game(main_size, main_matrix, main_moves);
  writeln(IntToStr(main_score));
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
