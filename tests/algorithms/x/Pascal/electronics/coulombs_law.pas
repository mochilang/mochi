{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  COULOMBS_CONSTANT: real;
  print_map_k_idx: integer;
  distance: real;
  force: real;
  m: specialize TFPGMap<string, real>;
  x: real;
  charge2: real;
  charge1: real;
function Map4(coulombs_law_d: real): specialize TFPGMap<string, real>; forward;
function Map3(coulombs_law_c2: real): specialize TFPGMap<string, real>; forward;
function Map2(coulombs_law_c1: real): specialize TFPGMap<string, real>; forward;
function Map1(coulombs_law_f: real): specialize TFPGMap<string, real>; forward;
function abs(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function coulombs_law(force: real; charge1: real; charge2: real; distance: real): specialize TFPGMap<string, real>; forward;
procedure print_map(m: specialize TFPGMap<string, real>); forward;
function Map4(coulombs_law_d: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('distance', Variant(coulombs_law_d));
end;
function Map3(coulombs_law_c2: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('charge2', Variant(coulombs_law_c2));
end;
function Map2(coulombs_law_c1: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('charge1', Variant(coulombs_law_c1));
end;
function Map1(coulombs_law_f: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('force', Variant(coulombs_law_f));
end;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function coulombs_law(force: real; charge1: real; charge2: real; distance: real): specialize TFPGMap<string, real>;
var
  coulombs_law_charge_product: real;
  coulombs_law_zero_count: integer;
  coulombs_law_f: real;
  coulombs_law_c1: real;
  coulombs_law_c2: real;
  coulombs_law_d: real;
begin
  coulombs_law_charge_product := abs(charge1 * charge2);
  coulombs_law_zero_count := 0;
  if force = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if charge1 = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if charge2 = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if distance = 0 then begin
  coulombs_law_zero_count := coulombs_law_zero_count + 1;
end;
  if coulombs_law_zero_count <> 1 then begin
  panic('One and only one argument must be 0');
end;
  if distance < 0 then begin
  panic('Distance cannot be negative');
end;
  if force = 0 then begin
  coulombs_law_f := (COULOMBS_CONSTANT * coulombs_law_charge_product) / (distance * distance);
  exit(Map1(coulombs_law_f));
end;
  if charge1 = 0 then begin
  coulombs_law_c1 := (abs(force) * (distance * distance)) / (COULOMBS_CONSTANT * charge2);
  exit(Map2(coulombs_law_c1));
end;
  if charge2 = 0 then begin
  coulombs_law_c2 := (abs(force) * (distance * distance)) / (COULOMBS_CONSTANT * charge1);
  exit(Map3(coulombs_law_c2));
end;
  coulombs_law_d := sqrtApprox((COULOMBS_CONSTANT * coulombs_law_charge_product) / abs(force));
  exit(Map4(coulombs_law_d));
end;
procedure print_map(m: specialize TFPGMap<string, real>);
var
  print_map_k: string;
begin
  for print_map_k_idx := 0 to (m.Count - 1) do begin
  print_map_k := m.Keys[print_map_k_idx];
  writeln(((('{"' + print_map_k) + '": ') + FloatToStr(m[print_map_k])) + '}');
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
end.
