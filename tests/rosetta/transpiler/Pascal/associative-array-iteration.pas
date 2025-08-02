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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function Map1(): specialize TFPGMap<string, integer>; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('hello', 13);
  Result.AddOrSetData('world', 31);
  Result.AddOrSetData('!', 71);
end;
procedure main();
var
  main_m: specialize TFPGMap<string, integer>;
  main_k: string;
  main_k_idx: integer;
begin
  main_m := Map1();
  for main_k_idx := 0 to (main_m.Count - 1) do begin
  main_k := main_m.Keys[main_k_idx];
  writeln((('key = ' + main_k) + ', value = ') + IntToStr(main_m[main_k]));
end;
  for main_k_idx := 0 to (main_m.Count - 1) do begin
  main_k := main_m.Keys[main_k_idx];
  writeln('key = ' + main_k);
end;
  for main_k_idx := 0 to (main_m.Count - 1) do begin
  main_k := main_m.Keys[main_k_idx];
  writeln('value = ' + IntToStr(main_m[main_k]));
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
