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
  seed: integer;
  NEG_ONE: integer;
  initial_speed: integer;
  probability: real;
  number_of_update: integer;
  a: integer;
  number_of_cells: integer;
  highway_now: IntArray;
  highway: IntArrayArray;
  car_index: integer;
  frequency: integer;
  b: integer;
  max_speed: integer;
function rand(): integer; forward;
function randint(a: integer; b: integer): integer; forward;
function random(): real; forward;
function construct_highway(number_of_cells: integer; frequency: integer; initial_speed: integer; random_frequency: boolean; random_speed: boolean; max_speed: integer): IntArrayArray; forward;
function get_distance(highway_now: IntArray; car_index: integer): integer; forward;
function update(highway_now: IntArray; probability: real; max_speed: integer): IntArray; forward;
function simulate(highway: IntArrayArray; number_of_update: integer; probability: real; max_speed: integer): IntArrayArray; forward;
procedure main(); forward;
function rand(): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed);
end;
function randint(a: integer; b: integer): integer;
var
  randint_r: integer;
begin
  randint_r := rand();
  exit(a + (randint_r mod ((b - a) + 1)));
end;
function random(): real;
var
  random_speed: boolean;
  random_frequency: boolean;
begin
  exit((1 * rand()) / 2.147483648e+09);
end;
function construct_highway(number_of_cells: integer; frequency: integer; initial_speed: integer; random_frequency: boolean; random_speed: boolean; max_speed: integer): IntArrayArray;
var
  construct_highway_row: array of integer;
  construct_highway_i: integer;
  construct_highway_highway: array of IntArray;
  construct_highway_speed: integer;
  construct_highway_step: integer;
begin
  construct_highway_row := [];
  construct_highway_i := 0;
  while construct_highway_i < number_of_cells do begin
  construct_highway_row := concat(construct_highway_row, IntArray([-1]));
  construct_highway_i := construct_highway_i + 1;
end;
  construct_highway_highway := [];
  construct_highway_highway := concat(construct_highway_highway, [construct_highway_row]);
  construct_highway_i := 0;
  if initial_speed < 0 then begin
  initial_speed := 0;
end;
  while construct_highway_i < number_of_cells do begin
  construct_highway_speed := initial_speed;
  if random_speed then begin
  construct_highway_speed := randint(0, max_speed);
end;
  construct_highway_highway[0][construct_highway_i] := construct_highway_speed;
  construct_highway_step := frequency;
  if random_frequency then begin
  construct_highway_step := randint(1, max_speed * 2);
end;
  construct_highway_i := construct_highway_i + construct_highway_step;
end;
  exit(construct_highway_highway);
end;
function get_distance(highway_now: IntArray; car_index: integer): integer;
var
  get_distance_distance: integer;
  get_distance_i: integer;
begin
  get_distance_distance := 0;
  get_distance_i := car_index + 1;
  while get_distance_i < Length(highway_now) do begin
  if highway_now[get_distance_i] > NEG_ONE then begin
  exit(get_distance_distance);
end;
  get_distance_distance := get_distance_distance + 1;
  get_distance_i := get_distance_i + 1;
end;
  exit(get_distance_distance + get_distance(highway_now, -1));
end;
function update(highway_now: IntArray; probability: real; max_speed: integer): IntArray;
var
  update_number_of_cells: integer;
  update_next_highway: array of integer;
  update_i: integer;
  update_car_index: integer;
  update_speed: integer;
  update_new_speed: integer;
  update_dn: integer;
begin
  update_number_of_cells := Length(highway_now);
  update_next_highway := [];
  update_i := 0;
  while update_i < update_number_of_cells do begin
  update_next_highway := concat(update_next_highway, IntArray([-1]));
  update_i := update_i + 1;
end;
  update_car_index := 0;
  while update_car_index < update_number_of_cells do begin
  update_speed := highway_now[update_car_index];
  if update_speed > NEG_ONE then begin
  update_new_speed := update_speed + 1;
  if update_new_speed > max_speed then begin
  update_new_speed := max_speed;
end;
  update_dn := get_distance(highway_now, update_car_index) - 1;
  if update_new_speed > update_dn then begin
  update_new_speed := update_dn;
end;
  if random() < probability then begin
  update_new_speed := update_new_speed - 1;
  if update_new_speed < 0 then begin
  update_new_speed := 0;
end;
end;
  update_next_highway[update_car_index] := update_new_speed;
end;
  update_car_index := update_car_index + 1;
end;
  exit(update_next_highway);
end;
function simulate(highway: IntArrayArray; number_of_update: integer; probability: real; max_speed: integer): IntArrayArray;
var
  simulate_number_of_cells: integer;
  simulate_i: integer;
  simulate_next_speeds: IntArray;
  simulate_real_next: array of integer;
  simulate_j: integer;
  simulate_k: integer;
  simulate_speed: integer;
  simulate_index: integer;
begin
  simulate_number_of_cells := Length(highway[0]);
  simulate_i := 0;
  while simulate_i < number_of_update do begin
  simulate_next_speeds := update(highway[simulate_i], probability, max_speed);
  simulate_real_next := [];
  simulate_j := 0;
  while simulate_j < simulate_number_of_cells do begin
  simulate_real_next := concat(simulate_real_next, IntArray([-1]));
  simulate_j := simulate_j + 1;
end;
  simulate_k := 0;
  while simulate_k < simulate_number_of_cells do begin
  simulate_speed := simulate_next_speeds[simulate_k];
  if simulate_speed > NEG_ONE then begin
  simulate_index := (simulate_k + simulate_speed) mod simulate_number_of_cells;
  simulate_real_next[simulate_index] := simulate_speed;
end;
  simulate_k := simulate_k + 1;
end;
  highway := concat(highway, [simulate_real_next]);
  simulate_i := simulate_i + 1;
end;
  exit(highway);
end;
procedure main();
var
  main_ex1: IntArrayArray;
  main_ex2: IntArrayArray;
begin
  main_ex1 := simulate(construct_highway(6, 3, 0, false, false, 2), 2, 0, 2);
  writeln(list_list_int_to_str(main_ex1));
  main_ex2 := simulate(construct_highway(5, 2, -2, false, false, 2), 3, 0, 2);
  writeln(list_list_int_to_str(main_ex2));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  NEG_ONE := -1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
