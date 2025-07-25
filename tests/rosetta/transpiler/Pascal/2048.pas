{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
type Board = record
  cells: array of IntArray;
end;
type SpawnResult = record
  board: Board;
  full: boolean;
end;
type SlideResult = record
  row: array of integer;
  gain: integer;
end;
type MoveResult = record
  board: Board;
  score: integer;
  moved: boolean;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  SIZE: integer;
  newBoard_b: array of IntArray;
  newBoard_y: integer;
  newBoard_row: array of integer;
  newBoard_x: integer;
  spawnTile_grid: array of IntArray;
  spawnTile_empty: array of IntArray;
  spawnTile_y: integer;
  spawnTile_x: integer;
  spawnTile_idx: integer;
  spawnTile_cell: IntArray;
  spawnTile_val: integer;
  pad_s: string;
  pad_pad_var: integer;
  pad_i: integer;
  pad_out: string;
  draw_y: integer;
  draw_line: string;
  draw_x: integer;
  draw_v: integer;
  reverseRow_out: array of integer;
  reverseRow_i: integer;
  slideLeft_xs: array of integer;
  slideLeft_i: integer;
  slideLeft_res: array of integer;
  slideLeft_gain: integer;
  slideLeft_v: integer;
  moveLeft_grid: array of IntArray;
  moveLeft_moved: boolean;
  moveLeft_y: integer;
  moveLeft_r: SlideResult;
  moveLeft_new: array of integer;
  moveLeft_x: integer;
  moveRight_grid: array of IntArray;
  moveRight_moved: boolean;
  moveRight_y: integer;
  moveRight_rev: IntArray;
  moveRight_r: SlideResult;
  moveRight_x: integer;
  getCol_col: array of integer;
  getCol_y: integer;
  setCol_rows: array of IntArray;
  setCol_y: integer;
  setCol_row: IntArray;
  moveUp_grid: array of IntArray;
  moveUp_moved: boolean;
  moveUp_x: integer;
  moveUp_col: IntArray;
  moveUp_r: SlideResult;
  moveUp_new: array of integer;
  moveUp_y: integer;
  moveDown_grid: array of IntArray;
  moveDown_moved: boolean;
  moveDown_x: integer;
  moveDown_col: IntArray;
  moveDown_r: SlideResult;
  moveDown_y: integer;
  hasMoves_y: integer;
  hasMoves_x: integer;
  has2048_y: integer;
  has2048_x: integer;
  board_var: Board;
  r: SpawnResult;
  full: boolean;
  score: integer;
  cmd: string;
  moved: boolean;
  m: MoveResult;
  r2: SpawnResult;
function makeMoveResult(board: Board; score: integer; moved: boolean): MoveResult;
begin
  Result.board := board;
  Result.score := score;
  Result.moved := moved;
end;
function makeSlideResult(row: IntArray; gain: integer): SlideResult;
begin
  Result.row := row;
  Result.gain := gain;
end;
function makeSpawnResult(board: Board; full: boolean): SpawnResult;
begin
  Result.board := board;
  Result.full := full;
end;
function makeBoard(cells: IntArrayArray): Board;
begin
  Result.cells := cells;
end;
function newBoard(): Board;
begin
  newBoard_b := [];
  newBoard_y := 0;
  while newBoard_y < SIZE do begin
  newBoard_row := [];
  newBoard_x := 0;
  while newBoard_x < SIZE do begin
  newBoard_row := concat(newBoard_row, [0]);
  newBoard_x := newBoard_x + 1;
end;
  newBoard_b := concat(newBoard_b, [newBoard_row]);
  newBoard_y := newBoard_y + 1;
end;
  exit(makeBoard(newBoard_b));
end;
function spawnTile(b: Board): SpawnResult;
begin
  spawnTile_grid := b.cells;
  spawnTile_empty := [];
  spawnTile_y := 0;
  while spawnTile_y < SIZE do begin
  spawnTile_x := 0;
  while spawnTile_x < SIZE do begin
  if spawnTile_grid[spawnTile_y][spawnTile_x] = 0 then begin
  spawnTile_empty := concat(spawnTile_empty, [[spawnTile_x, spawnTile_y]]);
end;
  spawnTile_x := spawnTile_x + 1;
end;
  spawnTile_y := spawnTile_y + 1;
end;
  if Length(spawnTile_empty) = 0 then begin
  exit(makeSpawnResult(b, true));
end;
  spawnTile_idx := _now() mod Length(spawnTile_empty);
  spawnTile_cell := spawnTile_empty[spawnTile_idx];
  spawnTile_val := 4;
  if (_now() mod 10) < 9 then begin
  spawnTile_val := 2;
end;
  spawnTile_grid[spawnTile_cell[1]][spawnTile_cell[0]] := spawnTile_val;
  exit(makeSpawnResult(makeBoard(spawnTile_grid), Length(spawnTile_empty) = 1));
end;
function pad(n: integer): string;
begin
  pad_s := IntToStr(n);
  pad_pad_var := 4 - Length(pad_s);
  pad_i := 0;
  pad_out := '';
  while pad_i < pad_pad_var do begin
  pad_out := pad_out + ' ';
  pad_i := pad_i + 1;
end;
  exit(pad_out + pad_s);
end;
procedure draw(b: Board; score: integer);
begin
  writeln('Score: ' + IntToStr(score));
  draw_y := 0;
  while draw_y < SIZE do begin
  writeln('+----+----+----+----+');
  draw_line := '|';
  draw_x := 0;
  while draw_x < SIZE do begin
  draw_v := b.cells[draw_y][draw_x];
  if draw_v = 0 then begin
  draw_line := draw_line + '    |';
end else begin
  draw_line := (draw_line + pad(draw_v)) + '|';
end;
  draw_x := draw_x + 1;
end;
  writeln(draw_line);
  draw_y := draw_y + 1;
end;
  writeln('+----+----+----+----+');
  writeln('W=Up S=Down A=Left D=Right Q=Quit');
end;
function reverseRow(r: IntArray): IntArray;
begin
  reverseRow_out := [];
  reverseRow_i := Length(r) - 1;
  while reverseRow_i >= 0 do begin
  reverseRow_out := concat(reverseRow_out, [r[reverseRow_i]]);
  reverseRow_i := reverseRow_i - 1;
end;
  exit(reverseRow_out);
end;
function slideLeft(row: IntArray): SlideResult;
begin
  slideLeft_xs := [];
  slideLeft_i := 0;
  while slideLeft_i < Length(row) do begin
  if row[slideLeft_i] <> 0 then begin
  slideLeft_xs := concat(slideLeft_xs, [row[slideLeft_i]]);
end;
  slideLeft_i := slideLeft_i + 1;
end;
  slideLeft_res := [];
  slideLeft_gain := 0;
  slideLeft_i := 0;
  while slideLeft_i < Length(slideLeft_xs) do begin
  if ((slideLeft_i + 1) < Length(slideLeft_xs)) and (slideLeft_xs[slideLeft_i] = slideLeft_xs[slideLeft_i + 1]) then begin
  slideLeft_v := slideLeft_xs[slideLeft_i] * 2;
  slideLeft_gain := slideLeft_gain + slideLeft_v;
  slideLeft_res := concat(slideLeft_res, [slideLeft_v]);
  slideLeft_i := slideLeft_i + 2;
end else begin
  slideLeft_res := concat(slideLeft_res, [slideLeft_xs[slideLeft_i]]);
  slideLeft_i := slideLeft_i + 1;
end;
end;
  while Length(slideLeft_res) < SIZE do begin
  slideLeft_res := concat(slideLeft_res, [0]);
end;
  exit(makeSlideResult(slideLeft_res, slideLeft_gain));
end;
function moveLeft(b: Board; score: integer): MoveResult;
begin
  moveLeft_grid := b.cells;
  moveLeft_moved := false;
  moveLeft_y := 0;
  while moveLeft_y < SIZE do begin
  moveLeft_r := slideLeft(moveLeft_grid[moveLeft_y]);
  moveLeft_new := moveLeft_r.row;
  score := score + moveLeft_r.gain;
  moveLeft_x := 0;
  while moveLeft_x < SIZE do begin
  if moveLeft_grid[moveLeft_y][moveLeft_x] <> moveLeft_new[moveLeft_x] then begin
  moveLeft_moved := true;
end;
  moveLeft_grid[moveLeft_y][moveLeft_x] := moveLeft_new[moveLeft_x];
  moveLeft_x := moveLeft_x + 1;
end;
  moveLeft_y := moveLeft_y + 1;
end;
  exit(makeMoveResult(makeBoard(moveLeft_grid), score, moveLeft_moved));
end;
function moveRight(b: Board; score: integer): MoveResult;
begin
  moveRight_grid := b.cells;
  moveRight_moved := false;
  moveRight_y := 0;
  while moveRight_y < SIZE do begin
  moveRight_rev := reverseRow(moveRight_grid[moveRight_y]);
  moveRight_r := slideLeft(moveRight_rev);
  moveRight_rev := moveRight_r.row;
  score := score + moveRight_r.gain;
  moveRight_rev := reverseRow(moveRight_rev);
  moveRight_x := 0;
  while moveRight_x < SIZE do begin
  if moveRight_grid[moveRight_y][moveRight_x] <> moveRight_rev[moveRight_x] then begin
  moveRight_moved := true;
end;
  moveRight_grid[moveRight_y][moveRight_x] := moveRight_rev[moveRight_x];
  moveRight_x := moveRight_x + 1;
end;
  moveRight_y := moveRight_y + 1;
end;
  exit(makeMoveResult(makeBoard(moveRight_grid), score, moveRight_moved));
end;
function getCol(b: Board; x: integer): IntArray;
begin
  getCol_col := [];
  getCol_y := 0;
  while getCol_y < SIZE do begin
  getCol_col := concat(getCol_col, [b.cells[getCol_y][x]]);
  getCol_y := getCol_y + 1;
end;
  exit(getCol_col);
end;
procedure setCol(b: Board; x: integer; col: IntArray);
begin
  setCol_rows := b.cells;
  setCol_y := 0;
  while setCol_y < SIZE do begin
  setCol_row := setCol_rows[setCol_y];
  setCol_row[x] := col[setCol_y];
  setCol_rows[setCol_y] := setCol_row;
  setCol_y := setCol_y + 1;
end;
  b.cells := setCol_rows;
end;
function moveUp(b: Board; score: integer): MoveResult;
begin
  moveUp_grid := b.cells;
  moveUp_moved := false;
  moveUp_x := 0;
  while moveUp_x < SIZE do begin
  moveUp_col := getCol(b, moveUp_x);
  moveUp_r := slideLeft(moveUp_col);
  moveUp_new := moveUp_r.row;
  score := score + moveUp_r.gain;
  moveUp_y := 0;
  while moveUp_y < SIZE do begin
  if moveUp_grid[moveUp_y][moveUp_x] <> moveUp_new[moveUp_y] then begin
  moveUp_moved := true;
end;
  moveUp_grid[moveUp_y][moveUp_x] := moveUp_new[moveUp_y];
  moveUp_y := moveUp_y + 1;
end;
  moveUp_x := moveUp_x + 1;
end;
  exit(makeMoveResult(makeBoard(moveUp_grid), score, moveUp_moved));
end;
function moveDown(b: Board; score: integer): MoveResult;
begin
  moveDown_grid := b.cells;
  moveDown_moved := false;
  moveDown_x := 0;
  while moveDown_x < SIZE do begin
  moveDown_col := reverseRow(getCol(b, moveDown_x));
  moveDown_r := slideLeft(moveDown_col);
  moveDown_col := moveDown_r.row;
  score := score + moveDown_r.gain;
  moveDown_col := reverseRow(moveDown_col);
  moveDown_y := 0;
  while moveDown_y < SIZE do begin
  if moveDown_grid[moveDown_y][moveDown_x] <> moveDown_col[moveDown_y] then begin
  moveDown_moved := true;
end;
  moveDown_grid[moveDown_y][moveDown_x] := moveDown_col[moveDown_y];
  moveDown_y := moveDown_y + 1;
end;
  moveDown_x := moveDown_x + 1;
end;
  exit(makeMoveResult(makeBoard(moveDown_grid), score, moveDown_moved));
end;
function hasMoves(b: Board): boolean;
begin
  hasMoves_y := 0;
  while hasMoves_y < SIZE do begin
  hasMoves_x := 0;
  while hasMoves_x < SIZE do begin
  if b.cells[hasMoves_y][hasMoves_x] = 0 then begin
  exit(true);
end;
  if ((hasMoves_x + 1) < SIZE) and (b.cells[hasMoves_y][hasMoves_x] = b.cells[hasMoves_y][hasMoves_x + 1]) then begin
  exit(true);
end;
  if ((hasMoves_y + 1) < SIZE) and (b.cells[hasMoves_y][hasMoves_x] = b.cells[hasMoves_y + 1][hasMoves_x]) then begin
  exit(true);
end;
  hasMoves_x := hasMoves_x + 1;
end;
  hasMoves_y := hasMoves_y + 1;
end;
  exit(false);
end;
function has2048(b: Board): boolean;
begin
  has2048_y := 0;
  while has2048_y < SIZE do begin
  has2048_x := 0;
  while has2048_x < SIZE do begin
  if b.cells[has2048_y][has2048_x] >= 2048 then begin
  exit(true);
end;
  has2048_x := has2048_x + 1;
end;
  has2048_y := has2048_y + 1;
end;
  exit(false);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _now();
  SIZE := 4;
  board_var := newBoard();
  r := spawnTile(board_var);
  board_var := r.board;
  full := r.full;
  r := spawnTile(board_var);
  board_var := r.board;
  full := r.full;
  score := 0;
  draw(board_var, score);
  while true do begin
  writeln('Move: ');
  cmd := _input();
  moved := false;
  if (cmd = 'a') or (cmd = 'A') then begin
  m := moveLeft(board_var, score);
  board_var := m.board;
  score := m.score;
  moved := m.moved;
end;
  if (cmd = 'd') or (cmd = 'D') then begin
  m := moveRight(board_var, score);
  board_var := m.board;
  score := m.score;
  moved := m.moved;
end;
  if (cmd = 'w') or (cmd = 'W') then begin
  m := moveUp(board_var, score);
  board_var := m.board;
  score := m.score;
  moved := m.moved;
end;
  if (cmd = 's') or (cmd = 'S') then begin
  m := moveDown(board_var, score);
  board_var := m.board;
  score := m.score;
  moved := m.moved;
end;
  if (cmd = 'q') or (cmd = 'Q') then begin
  break;
end;
  if moved then begin
  r2 := spawnTile(board_var);
  board_var := r2.board;
  full := r2.full;
  if full and not hasMoves(board_var) then begin
  draw(board_var, score);
  writeln('Game Over');
  break;
end;
end;
  draw(board_var, score);
  if has2048(board_var) then begin
  writeln('You win!');
  break;
end;
  if not hasMoves(board_var) then begin
  writeln('Game Over');
  break;
end;
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
