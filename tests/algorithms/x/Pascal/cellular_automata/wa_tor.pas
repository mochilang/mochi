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
  WIDTH: integer;
  HEIGHT: integer;
  PREY_INITIAL_COUNT: integer;
  PREY_REPRODUCTION_TIME: integer;
  PREDATOR_INITIAL_COUNT: integer;
  PREDATOR_REPRODUCTION_TIME: integer;
  PREDATOR_INITIAL_ENERGY: integer;
  PREDATOR_FOOD_VALUE: integer;
  TYPE_PREY: integer;
  TYPE_PREDATOR: integer;
  seed: integer;
  board: IntArrayArray;
  entities: array of IntArray;
  dr: array of integer;
  dc: array of integer;
  t: integer;
  typ: integer;
  max: integer;
  c: integer;
  r: integer;
  list_int: IntArray;
function rand(): integer; forward;
function rand_range(max: integer): integer; forward;
function shuffle(list_int: IntArray): IntArray; forward;
function create_board(): IntArrayArray; forward;
function create_prey(r: integer; c: integer): IntArray; forward;
function create_predator(r: integer; c: integer): IntArray; forward;
function empty_cell(r: integer; c: integer): boolean; forward;
procedure add_entity(typ: integer); forward;
procedure setup(); forward;
function inside(r: integer; c: integer): boolean; forward;
function find_prey(r: integer; c: integer): integer; forward;
procedure step_world(); forward;
function count_entities(typ: integer): integer; forward;
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
function create_board(): IntArrayArray;
var
  create_board_board: array of IntArray;
  create_board_r: integer;
  create_board_row: array of integer;
  create_board_c: integer;
begin
  create_board_board := [];
  create_board_r := 0;
  while create_board_r < HEIGHT do begin
  create_board_row := [];
  create_board_c := 0;
  while create_board_c < WIDTH do begin
  create_board_row := concat(create_board_row, IntArray([0]));
  create_board_c := create_board_c + 1;
end;
  create_board_board := concat(create_board_board, [create_board_row]);
  create_board_r := create_board_r + 1;
end;
  exit(create_board_board);
end;
function create_prey(r: integer; c: integer): IntArray;
begin
  exit([TYPE_PREY, r, c, PREY_REPRODUCTION_TIME, 0, 1]);
end;
function create_predator(r: integer; c: integer): IntArray;
begin
  exit([TYPE_PREDATOR, r, c, PREDATOR_REPRODUCTION_TIME, PREDATOR_INITIAL_ENERGY, 1]);
end;
function empty_cell(r: integer; c: integer): boolean;
begin
  exit(board[r][c] = 0);
end;
procedure add_entity(typ: integer);
var
  add_entity_r: integer;
  add_entity_c: integer;
begin
  while true do begin
  add_entity_r := rand_range(HEIGHT);
  add_entity_c := rand_range(WIDTH);
  if empty_cell(add_entity_r, add_entity_c) then begin
  if typ = TYPE_PREY then begin
  board[add_entity_r][add_entity_c] := 1;
  entities := concat(entities, [create_prey(add_entity_r, add_entity_c)]);
end else begin
  board[add_entity_r][add_entity_c] := 2;
  entities := concat(entities, [create_predator(add_entity_r, add_entity_c)]);
end;
  exit();
end;
end;
end;
procedure setup();
var
  setup_i: integer;
begin
  setup_i := 0;
  while setup_i < PREY_INITIAL_COUNT do begin
  add_entity(TYPE_PREY);
  setup_i := setup_i + 1;
end;
  setup_i := 0;
  while setup_i < PREDATOR_INITIAL_COUNT do begin
  add_entity(TYPE_PREDATOR);
  setup_i := setup_i + 1;
end;
end;
function inside(r: integer; c: integer): boolean;
begin
  exit((((r >= 0) and (r < HEIGHT)) and (c >= 0)) and (c < WIDTH));
end;
function find_prey(r: integer; c: integer): integer;
var
  find_prey_i: integer;
  find_prey_e: array of integer;
begin
  find_prey_i := 0;
  while find_prey_i < Length(entities) do begin
  find_prey_e := entities[find_prey_i];
  if (((find_prey_e[5] = 1) and (find_prey_e[0] = TYPE_PREY)) and (find_prey_e[1] = r)) and (find_prey_e[2] = c) then begin
  exit(find_prey_i);
end;
  find_prey_i := find_prey_i + 1;
end;
  exit(-1);
end;
procedure step_world();
var
  step_world_i: integer;
  step_world_e: array of integer;
  step_world_typ: integer;
  step_world_row: integer;
  step_world_col: integer;
  step_world_repro: integer;
  step_world_energy: integer;
  step_world_dirs: array of integer;
  step_world_moved: boolean;
  step_world_old_r: integer;
  step_world_old_c: integer;
  step_world_j: integer;
  step_world_ate: boolean;
  step_world_d: integer;
  step_world_nr: integer;
  step_world_nc: integer;
  step_world_prey_index: integer;
  step_world_alive: array of IntArray;
  step_world_k: integer;
  step_world_e2: array of integer;
begin
  step_world_i := 0;
  while step_world_i < Length(entities) do begin
  step_world_e := entities[step_world_i];
  if step_world_e[5] = 0 then begin
  step_world_i := step_world_i + 1;
  continue;
end;
  step_world_typ := step_world_e[0];
  step_world_row := step_world_e[1];
  step_world_col := step_world_e[2];
  step_world_repro := step_world_e[3];
  step_world_energy := step_world_e[4];
  step_world_dirs := [0, 1, 2, 3];
  step_world_dirs := shuffle(step_world_dirs);
  step_world_moved := false;
  step_world_old_r := step_world_row;
  step_world_old_c := step_world_col;
  if step_world_typ = TYPE_PREDATOR then begin
  step_world_j := 0;
  step_world_ate := false;
  while step_world_j < 4 do begin
  step_world_d := step_world_dirs[step_world_j];
  step_world_nr := step_world_row + dr[step_world_d];
  step_world_nc := step_world_col + dc[step_world_d];
  if inside(step_world_nr, step_world_nc) and (board[step_world_nr][step_world_nc] = 1) then begin
  step_world_prey_index := find_prey(step_world_nr, step_world_nc);
  if step_world_prey_index >= 0 then begin
  entities[step_world_prey_index][5] := 0;
end;
  board[step_world_nr][step_world_nc] := 2;
  board[step_world_row][step_world_col] := 0;
  step_world_e[1] := step_world_nr;
  step_world_e[2] := step_world_nc;
  step_world_e[4] := (step_world_energy + PREDATOR_FOOD_VALUE) - 1;
  step_world_moved := true;
  step_world_ate := true;
  break;
end;
  step_world_j := step_world_j + 1;
end;
  if not step_world_ate then begin
  step_world_j := 0;
  while step_world_j < 4 do begin
  step_world_d := step_world_dirs[step_world_j];
  step_world_nr := step_world_row + dr[step_world_d];
  step_world_nc := step_world_col + dc[step_world_d];
  if inside(step_world_nr, step_world_nc) and (board[step_world_nr][step_world_nc] = 0) then begin
  board[step_world_nr][step_world_nc] := 2;
  board[step_world_row][step_world_col] := 0;
  step_world_e[1] := step_world_nr;
  step_world_e[2] := step_world_nc;
  step_world_moved := true;
  break;
end;
  step_world_j := step_world_j + 1;
end;
  step_world_e[4] := step_world_energy - 1;
end;
  if step_world_e[4] <= 0 then begin
  step_world_e[5] := 0;
  board[step_world_e[1]][step_world_e[2]] := 0;
end;
end else begin
  step_world_j := 0;
  while step_world_j < 4 do begin
  step_world_d := step_world_dirs[step_world_j];
  step_world_nr := step_world_row + dr[step_world_d];
  step_world_nc := step_world_col + dc[step_world_d];
  if inside(step_world_nr, step_world_nc) and (board[step_world_nr][step_world_nc] = 0) then begin
  board[step_world_nr][step_world_nc] := 1;
  board[step_world_row][step_world_col] := 0;
  step_world_e[1] := step_world_nr;
  step_world_e[2] := step_world_nc;
  step_world_moved := true;
  break;
end;
  step_world_j := step_world_j + 1;
end;
end;
  if step_world_e[5] = 1 then begin
  if step_world_moved and (step_world_repro <= 0) then begin
  if step_world_typ = TYPE_PREY then begin
  board[step_world_old_r][step_world_old_c] := 1;
  entities := concat(entities, [create_prey(step_world_old_r, step_world_old_c)]);
  step_world_e[3] := PREY_REPRODUCTION_TIME;
end else begin
  board[step_world_old_r][step_world_old_c] := 2;
  entities := concat(entities, [create_predator(step_world_old_r, step_world_old_c)]);
  step_world_e[3] := PREDATOR_REPRODUCTION_TIME;
end;
end else begin
  step_world_e[3] := step_world_repro - 1;
end;
end;
  step_world_i := step_world_i + 1;
end;
  step_world_alive := [];
  step_world_k := 0;
  while step_world_k < Length(entities) do begin
  step_world_e2 := entities[step_world_k];
  if step_world_e2[5] = 1 then begin
  step_world_alive := concat(step_world_alive, [step_world_e2]);
end;
  step_world_k := step_world_k + 1;
end;
  entities := step_world_alive;
end;
function count_entities(typ: integer): integer;
var
  count_entities_cnt: integer;
  count_entities_i: integer;
begin
  count_entities_cnt := 0;
  count_entities_i := 0;
  while count_entities_i < Length(entities) do begin
  if (entities[count_entities_i][0] = typ) and (entities[count_entities_i][5] = 1) then begin
  count_entities_cnt := count_entities_cnt + 1;
end;
  count_entities_i := count_entities_i + 1;
end;
  exit(count_entities_cnt);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  WIDTH := 10;
  HEIGHT := 10;
  PREY_INITIAL_COUNT := 20;
  PREY_REPRODUCTION_TIME := 5;
  PREDATOR_INITIAL_COUNT := 5;
  PREDATOR_REPRODUCTION_TIME := 20;
  PREDATOR_INITIAL_ENERGY := 15;
  PREDATOR_FOOD_VALUE := 5;
  TYPE_PREY := 0;
  TYPE_PREDATOR := 1;
  seed := 123456789;
  board := create_board();
  entities := [];
  dr := [-1, 0, 1, 0];
  dc := [0, 1, 0, -1];
  setup();
  t := 0;
  while t < 10 do begin
  step_world();
  t := t + 1;
end;
  writeln('Prey: ' + IntToStr(count_entities(TYPE_PREY)));
  writeln('Predators: ' + IntToStr(count_entities(TYPE_PREDATOR)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
