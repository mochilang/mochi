{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type Body = record
  position_x: real;
  position_y: real;
  velocity_x: real;
  velocity_y: real;
  mass: real;
end;
type BodySystem = record
  bodies: array of Body;
  gravitation_constant: real;
  time_factor: real;
  softening_factor: real;
end;
type BodyArray = array of Body;
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
  bodies: BodyArray;
  vx: real;
  mass: real;
  tf: real;
  vy: real;
  force_x: real;
  px: real;
  body_var: Body;
  py: real;
  delta_time: real;
  force_y: real;
  x: real;
  sf: real;
  g: real;
  system: BodySystem;
function Map4(main_vel2x: real; main_vel2y: real): specialize TFPGMap<string, real>; forward;
function Map3(main_pos2x: real; main_pos2y: real): specialize TFPGMap<string, real>; forward;
function Map2(main_vel1x: real; main_vel1y: real): specialize TFPGMap<string, real>; forward;
function Map1(main_pos1x: real; main_pos1y: real): specialize TFPGMap<string, real>; forward;
function makeBodySystem(bodies: BodyArray; gravitation_constant: real; time_factor: real; softening_factor: real): BodySystem; forward;
function makeBody(position_x: real; position_y: real; velocity_x: real; velocity_y: real; mass: real): Body; forward;
function make_body(px: real; py: real; vx: real; vy: real; mass: real): Body; forward;
function update_velocity(body_var: Body; force_x: real; force_y: real; delta_time: real): Body; forward;
function update_position(body_var: Body; delta_time: real): Body; forward;
function make_body_system(bodies: BodyArray; g: real; tf: real; sf: real): BodySystem; forward;
function sqrtApprox(x: real): real; forward;
function update_system(system: BodySystem; delta_time: real): BodySystem; forward;
procedure main(); forward;
function Map4(main_vel2x: real; main_vel2y: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('vx', Variant(main_vel2x));
  Result.AddOrSetData('vy', Variant(main_vel2y));
end;
function Map3(main_pos2x: real; main_pos2y: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('x', Variant(main_pos2x));
  Result.AddOrSetData('y', Variant(main_pos2y));
end;
function Map2(main_vel1x: real; main_vel1y: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('vx', Variant(main_vel1x));
  Result.AddOrSetData('vy', Variant(main_vel1y));
end;
function Map1(main_pos1x: real; main_pos1y: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('x', Variant(main_pos1x));
  Result.AddOrSetData('y', Variant(main_pos1y));
end;
function makeBodySystem(bodies: BodyArray; gravitation_constant: real; time_factor: real; softening_factor: real): BodySystem;
begin
  Result.bodies := bodies;
  Result.gravitation_constant := gravitation_constant;
  Result.time_factor := time_factor;
  Result.softening_factor := softening_factor;
end;
function makeBody(position_x: real; position_y: real; velocity_x: real; velocity_y: real; mass: real): Body;
begin
  Result.position_x := position_x;
  Result.position_y := position_y;
  Result.velocity_x := velocity_x;
  Result.velocity_y := velocity_y;
  Result.mass := mass;
end;
function make_body(px: real; py: real; vx: real; vy: real; mass: real): Body;
begin
  exit(makeBody(px, py, vx, vy, mass));
end;
function update_velocity(body_var: Body; force_x: real; force_y: real; delta_time: real): Body;
begin
  body_var.velocity_x := body_var.velocity_x + (force_x * delta_time);
  body_var.velocity_y := body_var.velocity_y + (force_y * delta_time);
  exit(body_var);
end;
function update_position(body_var: Body; delta_time: real): Body;
begin
  body_var.position_x := body_var.position_x + (body_var.velocity_x * delta_time);
  body_var.position_y := body_var.position_y + (body_var.velocity_y * delta_time);
  exit(body_var);
end;
function make_body_system(bodies: BodyArray; g: real; tf: real; sf: real): BodySystem;
begin
  exit(makeBodySystem(bodies, g, tf, sf));
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function update_system(system: BodySystem; delta_time: real): BodySystem;
var
  update_system_bodies: array of Body;
  update_system_i: integer;
  update_system_body1: Body;
  update_system_force_x: real;
  update_system_force_y: real;
  update_system_j: integer;
  update_system_body2: Body;
  update_system_dif_x: real;
  update_system_dif_y: real;
  update_system_distance_sq: real;
  update_system_distance: real;
  update_system_denom: real;
  update_system_body_var: Body;
begin
  update_system_bodies := system.bodies;
  update_system_i := 0;
  while update_system_i < Length(update_system_bodies) do begin
  update_system_body1 := update_system_bodies[update_system_i];
  update_system_force_x := 0;
  update_system_force_y := 0;
  update_system_j := 0;
  while update_system_j < Length(update_system_bodies) do begin
  if update_system_i <> update_system_j then begin
  update_system_body2 := update_system_bodies[update_system_j];
  update_system_dif_x := update_system_body2.position_x - update_system_body1.position_x;
  update_system_dif_y := update_system_body2.position_y - update_system_body1.position_y;
  update_system_distance_sq := ((update_system_dif_x * update_system_dif_x) + (update_system_dif_y * update_system_dif_y)) + system.softening_factor;
  update_system_distance := sqrtApprox(update_system_distance_sq);
  update_system_denom := (update_system_distance * update_system_distance) * update_system_distance;
  update_system_force_x := update_system_force_x + (((system.gravitation_constant * update_system_body2.mass) * update_system_dif_x) / update_system_denom);
  update_system_force_y := update_system_force_y + (((system.gravitation_constant * update_system_body2.mass) * update_system_dif_y) / update_system_denom);
end;
  update_system_j := update_system_j + 1;
end;
  update_system_body1 := update_velocity(update_system_body1, update_system_force_x, update_system_force_y, delta_time * system.time_factor);
  update_system_bodies[update_system_i] := update_system_body1;
  update_system_i := update_system_i + 1;
end;
  update_system_i := 0;
  while update_system_i < Length(update_system_bodies) do begin
  update_system_body_var := update_system_bodies[update_system_i];
  update_system_body_var := update_position(update_system_body_var, delta_time * system.time_factor);
  update_system_bodies[update_system_i] := update_system_body_var;
  update_system_i := update_system_i + 1;
end;
  system.bodies := update_system_bodies;
  exit(system);
end;
procedure main();
var
  main_b1: Body;
  main_b2: Body;
  main_sys1: BodySystem;
  main_b1_after: Body;
  main_pos1x: real;
  main_pos1y: real;
  main_vel1x: real;
  main_vel1y: real;
  main_b3: Body;
  main_b4: Body;
  main_sys2: BodySystem;
  main_b2_after: Body;
  main_pos2x: real;
  main_pos2y: real;
  main_vel2x: real;
  main_vel2y: real;
begin
  main_b1 := make_body(0, 0, 0, 0, 1);
  main_b2 := make_body(10, 0, 0, 0, 1);
  main_sys1 := make_body_system([main_b1, main_b2], 1, 1, 0);
  main_sys1 := update_system(main_sys1, 1);
  main_b1_after := main_sys1.bodies[0];
  main_pos1x := main_b1_after.position_x;
  main_pos1y := main_b1_after.position_y;
  json(Map1(main_pos1x, main_pos1y));
  main_vel1x := main_b1_after.velocity_x;
  main_vel1y := main_b1_after.velocity_y;
  json(Map2(main_vel1x, main_vel1y));
  main_b3 := make_body(-10, 0, 0, 0, 1);
  main_b4 := make_body(10, 0, 0, 0, 4);
  main_sys2 := make_body_system([main_b3, main_b4], 1, 10, 0);
  main_sys2 := update_system(main_sys2, 1);
  main_b2_after := main_sys2.bodies[0];
  main_pos2x := main_b2_after.position_x;
  main_pos2y := main_b2_after.position_y;
  json(Map3(main_pos2x, main_pos2y));
  main_vel2x := main_b2_after.velocity_x;
  main_vel2y := main_b2_after.velocity_y;
  json(Map4(main_vel2x, main_vel2y));
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
