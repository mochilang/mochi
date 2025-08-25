{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  COULOMBS_CONSTANT: real;
  print_map_k_idx: integer;
function Map4(coulombs_law_d: real): specialize TFPGMap<string, real>; forward;
function Map3(coulombs_law_c2: real): specialize TFPGMap<string, real>; forward;
function Map2(coulombs_law_c1: real): specialize TFPGMap<string, real>; forward;
function Map1(coulombs_law_f: real): specialize TFPGMap<string, real>; forward;
function abs(abs_x: real): real; forward;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function coulombs_law(coulombs_law_force: real; coulombs_law_charge1: real; coulombs_law_charge2: real; coulombs_law_distance: real): specialize TFPGMap<string, real>; forward;
procedure print_map(print_map_m: specialize TFPGMap<string, real>); forward;
function Map4(coulombs_law_d: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('distance', coulombs_law_d);
end;
function Map3(coulombs_law_c2: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('charge2', coulombs_law_c2);
end;
function Map2(coulombs_law_c1: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('charge1', coulombs_law_c1);
end;
function Map1(coulombs_law_f: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('force', coulombs_law_f);
end;
function abs(abs_x: real): real;
begin
  if abs_x < 0 then begin
  exit(-abs_x);
end;
  exit(abs_x);
end;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: int64;
begin
  if sqrtApprox_x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := sqrtApprox_x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function coulombs_law(coulombs_law_force: real; coulombs_law_charge1: real; coulombs_law_charge2: real; coulombs_law_distance: real): specialize TFPGMap<string, real>;
var
  coulombs_law_charge_product: real;
  coulombs_law_zero_count: int64;
  coulombs_law_f: real;
  coulombs_law_c1: real;
  coulombs_law_c2: real;
  coulombs_law_d: real;
begin
  coulombs_law_charge_product := abs(coulombs_law_charge1 * coulombs_law_charge2);
  coulombs_law_zero_count := 0;
  if coulombs_law_force = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if coulombs_law_charge1 = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if coulombs_law_charge2 = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if coulombs_law_distance = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if coulombs_law_zero_count <> 1 then begin
  panic('One and only one argument must be 0');
end;
  if coulombs_law_distance < 0 then begin
  panic('Distance cannot be negative');
end;
  if coulombs_law_force = 0 then begin
  coulombs_law_f := (COULOMBS_CONSTANT * coulombs_law_charge_product) / (coulombs_law_distance * coulombs_law_distance);
  exit(Map1(coulombs_law_f));
end;
  if coulombs_law_charge1 = 0 then begin
  coulombs_law_c1 := (abs(coulombs_law_force) * (coulombs_law_distance * coulombs_law_distance)) / (COULOMBS_CONSTANT * coulombs_law_charge2);
  exit(Map2(coulombs_law_c1));
end;
  if coulombs_law_charge2 = 0 then begin
  coulombs_law_c2 := (abs(coulombs_law_force) * (coulombs_law_distance * coulombs_law_distance)) / (COULOMBS_CONSTANT * coulombs_law_charge1);
  exit(Map3(coulombs_law_c2));
end;
  coulombs_law_d := sqrtApprox((COULOMBS_CONSTANT * coulombs_law_charge_product) / abs(coulombs_law_force));
  exit(Map4(coulombs_law_d));
end;
procedure print_map(print_map_m: specialize TFPGMap<string, real>);
var
  print_map_k: string;
begin
  for print_map_k_idx := 0 to (print_map_m.Count - 1) do begin
  print_map_k := print_map_m.Keys[print_map_k_idx];
  writeln(((('{"' + print_map_k) + '": ') + FloatToStr(print_map_m[print_map_k])) + '}');
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  COULOMBS_CONSTANT := 8.988e+09;
  print_map(coulombs_law(0, 3, 5, 2000));
  print_map(coulombs_law(10, 3, 5, 0));
  print_map(coulombs_law(10, 0, 5, 2000));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
