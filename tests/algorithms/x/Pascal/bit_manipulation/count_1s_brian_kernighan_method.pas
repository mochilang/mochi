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
  number: integer;
function lowest_set_bit(n: integer): integer; forward;
function get_1s_count(number: integer): integer; forward;
function lowest_set_bit(n: integer): integer;
var
  lowest_set_bit_lb: integer;
begin
  lowest_set_bit_lb := 1;
  while (n mod (lowest_set_bit_lb * 2)) = 0 do begin
  lowest_set_bit_lb := lowest_set_bit_lb * 2;
end;
  exit(lowest_set_bit_lb);
end;
function get_1s_count(number: integer): integer;
var
  get_1s_count_n: integer;
  get_1s_count_count: integer;
begin
  if number < 0 then begin
  writeln('ValueError: Input must be a non-negative integer');
  exit(0);
end;
  get_1s_count_n := number;
  get_1s_count_count := 0;
  while get_1s_count_n > 0 do begin
  get_1s_count_n := get_1s_count_n - lowest_set_bit(get_1s_count_n);
  get_1s_count_count := get_1s_count_count + 1;
end;
  exit(get_1s_count_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(get_1s_count(25)));
  writeln(IntToStr(get_1s_count(37)));
  writeln(IntToStr(get_1s_count(21)));
  writeln(IntToStr(get_1s_count(58)));
  writeln(IntToStr(get_1s_count(0)));
  writeln(IntToStr(get_1s_count(256)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
