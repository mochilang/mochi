{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function poolPut(p: IntArray; x: integer): IntArray; forward;
function poolGet(p: IntArray): specialize TFPGMap<string, Variant>; forward;
function clearPool(p: IntArray): IntArray; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('pool', p);
  Result.AddOrSetData('val', 0);
end;
function poolPut(p: IntArray; x: integer): IntArray;
begin
  exit(concat(p, [x]));
end;
function poolGet(p: IntArray): specialize TFPGMap<string, Variant>;
var
  poolGet_idx: integer;
  poolGet_v: integer;
begin
  if Length(p) = 0 then begin
  writeln('pool empty');
  exit(Map1());
end;
  poolGet_idx := Length(p) - 1;
  poolGet_v := p[poolGet_idx];
  p := copy(p, 0, (poolGet_idx - (0)));
  exit(Map1());
end;
function clearPool(p: IntArray): IntArray;
begin
  exit([]);
end;
procedure main();
var
  main_pool: array of integer;
  main_i: integer;
  main_j: integer;
  main_res1: specialize TFPGMap<string, Variant>;
  main_res2: specialize TFPGMap<string, Variant>;
  main_res3: specialize TFPGMap<string, Variant>;
  main_res4: specialize TFPGMap<string, Variant>;
begin
  main_pool := [];
  main_i := 1;
  main_j := 2;
  writeln(IntToStr(main_i + main_j));
  main_pool := poolPut(main_pool, main_i);
  main_pool := poolPut(main_pool, main_j);
  main_i := 0;
  main_j := 0;
  main_res1 := poolGet(main_pool);
  main_pool := main_res1['pool'];
  main_i := Trunc(main_res1['val']);
  main_res2 := poolGet(main_pool);
  main_pool := main_res2['pool'];
  main_j := Trunc(main_res2['val']);
  main_i := 4;
  main_j := 5;
  writeln(IntToStr(main_i + main_j));
  main_pool := poolPut(main_pool, main_i);
  main_pool := poolPut(main_pool, main_j);
  main_i := 0;
  main_j := 0;
  main_pool := clearPool(main_pool);
  main_res3 := poolGet(main_pool);
  main_pool := main_res3['pool'];
  main_i := Trunc(main_res3['val']);
  main_res4 := poolGet(main_pool);
  main_pool := main_res4['pool'];
  main_j := Trunc(main_res4['val']);
  main_i := 7;
  main_j := 8;
  writeln(IntToStr(main_i + main_j));
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
