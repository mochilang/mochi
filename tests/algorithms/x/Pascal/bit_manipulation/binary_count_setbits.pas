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
  n: integer;
function binary_count_setbits(n: integer): integer; forward;
function binary_count_setbits(n: integer): integer;
var
  binary_count_setbits_count: integer;
  binary_count_setbits_value: integer;
begin
  if n < 0 then begin
  panic('Input value must be a non-negative integer');
end;
  binary_count_setbits_count := 0;
  binary_count_setbits_value := n;
  while binary_count_setbits_value > 0 do begin
  binary_count_setbits_count := binary_count_setbits_count + (binary_count_setbits_value mod 2);
  binary_count_setbits_value := binary_count_setbits_value div 2;
end;
  exit(binary_count_setbits_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(binary_count_setbits(25)));
  writeln(IntToStr(binary_count_setbits(36)));
  writeln(IntToStr(binary_count_setbits(16)));
  writeln(IntToStr(binary_count_setbits(58)));
  writeln(IntToStr(binary_count_setbits(0)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
