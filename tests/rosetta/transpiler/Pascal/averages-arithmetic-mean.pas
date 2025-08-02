{$mode objfpc}
program Main;
uses SysUtils, Variants, fgl;
type RealArray = array of real;
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
function Map2(mean_sum: real; v: RealArray): specialize TFPGMap<string, Variant>; forward;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function mean(v: RealArray): specialize TFPGMap<string, Variant>; forward;
procedure main(); forward;
function Map2(mean_sum: real; v: RealArray): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('ok', true);
  Result.AddOrSetData('mean', mean_sum / Double(Length(v)));
end;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('ok', false);
end;
function mean(v: RealArray): specialize TFPGMap<string, Variant>;
var
  mean_sum: real;
  mean_i: integer;
begin
  if Length(v) = 0 then begin
  exit(Map1());
end;
  mean_sum := 0;
  mean_i := 0;
  while mean_i < Length(v) do begin
  mean_sum := mean_sum + v[mean_i];
  mean_i := mean_i + 1;
end;
  exit(Map2(mean_sum, v));
end;
procedure main();
var
  main_sets: array of array of real;
  main_v: array of real;
  main_r: specialize TFPGMap<string, Variant>;
begin
  main_sets := [[], [3, 1, 4, 1, 5, 9], [1e+20, 3, 1, 4, 1, 5, 9, -1e+20], [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11], [10, 20, 30, 40, 50, -100, 4.7, -1100]];
  for main_v in main_sets do begin
  writeln('Vector: ' + list_real_to_str(main_v));
  main_r := mean(main_v);
  if main_r['ok'] then begin
  writeln((('Mean of ' + IntToStr(Length(main_v))) + ' numbers is ') + VarToStr(main_r['mean']));
end else begin
  writeln('Mean undefined');
end;
  writeln('');
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
