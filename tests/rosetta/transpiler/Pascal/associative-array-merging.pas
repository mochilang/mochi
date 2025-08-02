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
procedure show_map(m: specialize TFPGMap<string, Variant>);
var i: integer;
begin
  write('map[');
  for i := 0 to m.Count - 1 do begin
    write(m.Keys[i]);
    write(':');
    write(m.Data[i]);
    if i < m.Count - 1 then write(' ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function Map2(): specialize TFPGMap<string, Variant>; forward;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function merge(base: specialize TFPGMap<string, Variant>; update: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>; forward;
procedure main(); forward;
function Map2(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('price', 15.25);
  Result.AddOrSetData('color', 'red');
  Result.AddOrSetData('year', 1974);
end;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('name', 'Rocket Skates');
  Result.AddOrSetData('price', 12.75);
  Result.AddOrSetData('color', 'yellow');
end;
function merge(base: specialize TFPGMap<string, Variant>; update: specialize TFPGMap<string, Variant>): specialize TFPGMap<string, Variant>;
var
  merge_result: specialize TFPGMap<string, Variant>;
  merge_k: string;
  merge_k_idx: integer;
begin
  merge_result := specialize TFPGMap<string, Variant>.Create();
  for merge_k_idx := 0 to (base.Count - 1) do begin
  merge_k := base.Keys[merge_k_idx];
  merge_result.AddOrSetData(merge_k, base[merge_k]);
end;
  for merge_k_idx := 0 to (update.Count - 1) do begin
  merge_k := update.Keys[merge_k_idx];
  merge_result.AddOrSetData(merge_k, update[merge_k]);
end;
  exit(merge_result);
end;
procedure main();
var
  main_base: specialize TFPGMap<string, Variant>;
  main_update: specialize TFPGMap<string, Variant>;
  main_result: specialize TFPGMap<string, Variant>;
begin
  main_base := Map1();
  main_update := Map2();
  main_result := merge(main_base, main_update);
  show_map(main_result);
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
