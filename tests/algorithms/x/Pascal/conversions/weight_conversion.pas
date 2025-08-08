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
  KILOGRAM_CHART: specialize TFPGMap<string, real>;
  WEIGHT_TYPE_CHART: specialize TFPGMap<string, real>;
  exp: integer;
  value: real;
  to_type: string;
  from_type: string;
function Map1(): specialize TFPGMap<string, real>; forward;
function pow10(exp: integer): real; forward;
function weight_conversion(from_type: string; to_type: string; value: real): real; forward;
function Map1(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('kilogram', Variant(1));
  Result.AddOrSetData('gram', Variant(1000));
  Result.AddOrSetData('milligram', Variant(1e+06));
  Result.AddOrSetData('metric-ton', Variant(0.001));
  Result.AddOrSetData('long-ton', Variant(0.0009842073));
  Result.AddOrSetData('short-ton', Variant(0.0011023122));
  Result.AddOrSetData('pound', Variant(2.2046244202));
  Result.AddOrSetData('stone', Variant(0.1574731728));
  Result.AddOrSetData('ounce', Variant(35.273990723));
  Result.AddOrSetData('carrat', Variant(5000));
  Result.AddOrSetData('atomic-mass-unit', Variant(6.022136652 * pow10(26)));
end;
function pow10(exp: integer): real;
var
  pow10_result_: real;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  if exp >= 0 then begin
  pow10_i := 0;
  while pow10_i < exp do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
end else begin
  pow10_i := 0;
  while pow10_i < (0 - exp) do begin
  pow10_result_ := pow10_result_ / 10;
  pow10_i := pow10_i + 1;
end;
end;
  exit(pow10_result_);
end;
function weight_conversion(from_type: string; to_type: string; value: real): real;
var
  weight_conversion_has_to: boolean;
  weight_conversion_has_from: boolean;
begin
  weight_conversion_has_to := KILOGRAM_CHART.IndexOf(to_type) <> -1;
  weight_conversion_has_from := WEIGHT_TYPE_CHART.IndexOf(from_type) <> -1;
  if weight_conversion_has_to and weight_conversion_has_from then begin
  exit((value * KILOGRAM_CHART[to_type]) * WEIGHT_TYPE_CHART[from_type]);
end;
  writeln('Invalid ''from_type'' or ''to_type''');
  exit(0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  KILOGRAM_CHART := Map1();
  WEIGHT_TYPE_CHART := Map1();
  writeln(weight_conversion('kilogram', 'gram', 1));
  writeln(weight_conversion('gram', 'pound', 3));
  writeln(weight_conversion('ounce', 'kilogram', 3));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
