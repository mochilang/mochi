{$mode objfpc}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: array of real;
  b: array of real;
  sig: array of real;
  res: RealArray;
  k: integer;
function applyFilter(input: RealArray; a: RealArray; b: RealArray): RealArray; forward;
function applyFilter(input: RealArray; a: RealArray; b: RealArray): RealArray;
var
  applyFilter_out: array of real;
  applyFilter_scale: real;
  applyFilter_i: integer;
  applyFilter_tmp: real;
  applyFilter_j: integer;
begin
  applyFilter_out := [];
  applyFilter_scale := 1 / a[0];
  applyFilter_i := 0;
  while applyFilter_i < Length(input) do begin
  applyFilter_tmp := 0;
  applyFilter_j := 0;
  while (applyFilter_j <= applyFilter_i) and (applyFilter_j < Length(b)) do begin
  applyFilter_tmp := applyFilter_tmp + (b[applyFilter_j] * input[applyFilter_i - applyFilter_j]);
  applyFilter_j := applyFilter_j + 1;
end;
  applyFilter_j := 0;
  while (applyFilter_j < applyFilter_i) and ((applyFilter_j + 1) < Length(a)) do begin
  applyFilter_tmp := applyFilter_tmp - (a[applyFilter_j + 1] * applyFilter_out[(applyFilter_i - applyFilter_j) - 1]);
  applyFilter_j := applyFilter_j + 1;
end;
  applyFilter_out := concat(applyFilter_out, [applyFilter_tmp * applyFilter_scale]);
  applyFilter_i := applyFilter_i + 1;
end;
  exit(applyFilter_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  a := [1, -2.7756e-16, 0.33333333, -1.85e-17];
  b := [0.16666667, 0.5, 0.5, 0.16666667];
  sig := [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589];
  res := applyFilter(sig, a, b);
  k := 0;
  while k < Length(res) do begin
  writeln(res[k]);
  k := k + 1;
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
