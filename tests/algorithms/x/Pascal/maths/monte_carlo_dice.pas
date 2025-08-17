{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  lcg_seed: integer;
  num_dice: integer;
  num_throws: integer;
  x: real;
function lcg_rand(): integer; forward;
function roll(): integer; forward;
function round2(x: real): real; forward;
function throw_dice(num_throws: integer; num_dice: integer): RealArray; forward;
procedure main(); forward;
function lcg_rand(): integer;
begin
  lcg_seed := ((lcg_seed * 1103515245) + 12345) mod 2147483648;
  exit(lcg_seed);
end;
function roll(): integer;
var
  roll_rv: real;
  roll_r: real;
begin
  roll_rv := Double(lcg_rand());
  roll_r := (roll_rv * 6) / 2.147483648e+09;
  exit(1 + Trunc(roll_r));
end;
function round2(x: real): real;
var
  round2_y: real;
  round2_z: integer;
begin
  round2_y := (x * 100) + 0.5;
  round2_z := Trunc(round2_y);
  exit(Double(round2_z) / 100);
end;
function throw_dice(num_throws: integer; num_dice: integer): RealArray;
var
  throw_dice_count_of_sum: array of integer;
  throw_dice_max_sum: integer;
  throw_dice_i: integer;
  throw_dice_t: integer;
  throw_dice_s: integer;
  throw_dice_d: integer;
  throw_dice_probability: array of real;
  throw_dice_p: real;
begin
  throw_dice_count_of_sum := [];
  throw_dice_max_sum := (num_dice * 6) + 1;
  throw_dice_i := 0;
  while throw_dice_i < throw_dice_max_sum do begin
  throw_dice_count_of_sum := concat(throw_dice_count_of_sum, IntArray([0]));
  throw_dice_i := throw_dice_i + 1;
end;
  throw_dice_t := 0;
  while throw_dice_t < num_throws do begin
  throw_dice_s := 0;
  throw_dice_d := 0;
  while throw_dice_d < num_dice do begin
  throw_dice_s := throw_dice_s + roll();
  throw_dice_d := throw_dice_d + 1;
end;
  throw_dice_count_of_sum[throw_dice_s] := throw_dice_count_of_sum[throw_dice_s] + 1;
  throw_dice_t := throw_dice_t + 1;
end;
  throw_dice_probability := [];
  throw_dice_i := num_dice;
  while throw_dice_i < throw_dice_max_sum do begin
  throw_dice_p := (Double(throw_dice_count_of_sum[throw_dice_i]) * 100) / Double(num_throws);
  throw_dice_probability := concat(throw_dice_probability, [round2(throw_dice_p)]);
  throw_dice_i := throw_dice_i + 1;
end;
  exit(throw_dice_probability);
end;
procedure main();
var
  main_result_: RealArray;
begin
  lcg_seed := 1;
  main_result_ := throw_dice(10000, 2);
  writeln(list_real_to_str(main_result_));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lcg_seed := 1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
