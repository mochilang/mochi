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
  ENERGY_CONVERSION_var: specialize TFPGMap<string, real>;
  value: real;
  from_type: string;
  to_type: string;
function Map1(): specialize TFPGMap<string, real>; forward;
function energy_conversion(from_type: string; to_type: string; value: real): real; forward;
function Map1(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('joule', Variant(1));
  Result.AddOrSetData('kilojoule', Variant(1000));
  Result.AddOrSetData('megajoule', Variant(1e+06));
  Result.AddOrSetData('gigajoule', Variant(1e+09));
  Result.AddOrSetData('wattsecond', Variant(1));
  Result.AddOrSetData('watthour', Variant(3600));
  Result.AddOrSetData('kilowatthour', Variant(3.6e+06));
  Result.AddOrSetData('newtonmeter', Variant(1));
  Result.AddOrSetData('calorie_nutr', Variant(4186.8));
  Result.AddOrSetData('kilocalorie_nutr', Variant(4.1868e+06));
  Result.AddOrSetData('electronvolt', Variant(1.602176634e-19));
  Result.AddOrSetData('britishthermalunit_it', Variant(1055.05585));
  Result.AddOrSetData('footpound', Variant(1.355818));
end;
function energy_conversion(from_type: string; to_type: string; value: real): real;
begin
  if (ENERGY_CONVERSION_var.IndexOf(from_type) <> -1 = false) or (ENERGY_CONVERSION_var.IndexOf(to_type) <> -1 = false) then begin
  panic('Incorrect ''from_type'' or ''to_type''');
end;
  exit((value * ENERGY_CONVERSION_var[from_type]) / ENERGY_CONVERSION_var[to_type]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ENERGY_CONVERSION_var := Map1();
  writeln(FloatToStr(energy_conversion('joule', 'kilojoule', 1)));
  writeln(FloatToStr(energy_conversion('kilowatthour', 'joule', 10)));
  writeln(FloatToStr(energy_conversion('britishthermalunit_it', 'footpound', 1)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
