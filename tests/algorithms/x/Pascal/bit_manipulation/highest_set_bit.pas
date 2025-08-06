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
function highest_set_bit_position(number: integer): integer; forward;
function highest_set_bit_position(number: integer): integer;
var
  highest_set_bit_position_position: integer;
  highest_set_bit_position_n: integer;
begin
  if number < 0 then begin
  panic('number must be non-negative');
end;
  highest_set_bit_position_position := 0;
  highest_set_bit_position_n := number;
  while highest_set_bit_position_n > 0 do begin
  highest_set_bit_position_position := highest_set_bit_position_position + 1;
  highest_set_bit_position_n := highest_set_bit_position_n div 2;
end;
  exit(highest_set_bit_position_position);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(highest_set_bit_position(25)));
  writeln(IntToStr(highest_set_bit_position(37)));
  writeln(IntToStr(highest_set_bit_position(1)));
  writeln(IntToStr(highest_set_bit_position(4)));
  writeln(IntToStr(highest_set_bit_position(0)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
