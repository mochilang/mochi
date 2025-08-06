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
  number: integer;
function index_of_rightmost_set_bit(number: integer): integer; forward;
function index_of_rightmost_set_bit(number: integer): integer;
var
  index_of_rightmost_set_bit_n: integer;
  index_of_rightmost_set_bit_index: integer;
begin
  if number < 0 then begin
  panic('Input must be a non-negative integer');
end;
  if number = 0 then begin
  exit(-1);
end;
  index_of_rightmost_set_bit_n := number;
  index_of_rightmost_set_bit_index := 0;
  while (index_of_rightmost_set_bit_n mod 2) = 0 do begin
  index_of_rightmost_set_bit_n := index_of_rightmost_set_bit_n div 2;
  index_of_rightmost_set_bit_index := index_of_rightmost_set_bit_index + 1;
end;
  exit(index_of_rightmost_set_bit_index);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(index_of_rightmost_set_bit(0)));
  writeln(IntToStr(index_of_rightmost_set_bit(5)));
  writeln(IntToStr(index_of_rightmost_set_bit(36)));
  writeln(IntToStr(index_of_rightmost_set_bit(8)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
