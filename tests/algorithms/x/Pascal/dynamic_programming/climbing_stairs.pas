{$mode objfpc}
program Main;
uses SysUtils;
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
  number_of_steps: integer;
function climb_stairs(number_of_steps: integer): integer; forward;
function climb_stairs(number_of_steps: integer): integer;
var
  climb_stairs_previous: integer;
  climb_stairs_current: integer;
  climb_stairs_i: integer;
  climb_stairs_next: integer;
begin
  if number_of_steps <= 0 then begin
  panic('number_of_steps needs to be positive');
end;
  if number_of_steps = 1 then begin
  exit(1);
end;
  climb_stairs_previous := 1;
  climb_stairs_current := 1;
  climb_stairs_i := 0;
  while climb_stairs_i < (number_of_steps - 1) do begin
  climb_stairs_next := climb_stairs_current + climb_stairs_previous;
  climb_stairs_previous := climb_stairs_current;
  climb_stairs_current := climb_stairs_next;
  climb_stairs_i := climb_stairs_i + 1;
end;
  exit(climb_stairs_current);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(climb_stairs(3));
  writeln(climb_stairs(1));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
