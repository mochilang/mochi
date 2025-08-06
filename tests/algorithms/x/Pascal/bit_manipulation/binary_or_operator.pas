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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function binary_or(a: integer; b: integer): string; forward;
function binary_or(a: integer; b: integer): string;
var
  binary_or_res: string;
  binary_or_x: integer;
  binary_or_y: integer;
  binary_or_bit_a: integer;
  binary_or_bit_b: integer;
begin
  if (a < 0) or (b < 0) then begin
  exit('ValueError');
end;
  binary_or_res := '';
  binary_or_x := a;
  binary_or_y := b;
  while (binary_or_x > 0) or (binary_or_y > 0) do begin
  binary_or_bit_a := binary_or_x mod 2;
  binary_or_bit_b := binary_or_y mod 2;
  if (binary_or_bit_a = 1) or (binary_or_bit_b = 1) then begin
  binary_or_res := '1' + binary_or_res;
end else begin
  binary_or_res := '0' + binary_or_res;
end;
  binary_or_x := binary_or_x div 2;
  binary_or_y := binary_or_y div 2;
end;
  if binary_or_res = '' then begin
  binary_or_res := '0';
end;
  exit('0b' + binary_or_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(binary_or(25, 32));
  writeln(binary_or(37, 50));
  writeln(binary_or(21, 30));
  writeln(binary_or(58, 73));
  writeln(binary_or(0, 255));
  writeln(binary_or(0, 256));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
