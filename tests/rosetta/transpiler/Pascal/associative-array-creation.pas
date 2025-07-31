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
function Map2(): specialize TFPGMap<string, integer>; forward;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function removeKey(m: specialize TFPGMap<string, integer>; k: string): specialize TFPGMap<string, integer>; forward;
procedure main(); forward;
function Map2(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('foo', 2);
  Result.AddOrSetData('bar', 42);
  Result.AddOrSetData('baz', -1);
end;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
end;
function removeKey(m: specialize TFPGMap<string, integer>; k: string): specialize TFPGMap<string, integer>;
var
  removeKey_out: specialize TFPGMap<string, integer>;
  removeKey_key: integer;
begin
  for removeKey_key in m do begin
  if removeKey_key <> k then begin
  removeKey_out.AddOrSetData(removeKey_key, m[removeKey_key]);
end;
end;
  exit(removeKey_out);
end;
procedure main();
var
  main_x: specialize TFPGMap<string, integer>;
  main_y1: integer;
  main_ok: boolean;
begin
  main_x := nil;
  main_x := Map1();
  main_x.AddOrSetData('foo', 3);
  main_y1 := main_x['bar'];
  main_ok := main_x.IndexOf('bar') <> -1;
  writeln(main_y1);
  writeln(Ord(main_ok));
  main_x := removeKey(main_x, 'foo');
  main_x := Map2();
  writeln(main_x['foo'], ' ', main_x['bar'], ' ', main_x['baz']);
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
