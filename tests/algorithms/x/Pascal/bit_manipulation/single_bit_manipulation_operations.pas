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
  position: integer;
  exp: integer;
  number: integer;
function pow2(exp: integer): integer; forward;
function is_bit_set(number: integer; position: integer): boolean; forward;
function set_bit(number: integer; position: integer): integer; forward;
function clear_bit(number: integer; position: integer): integer; forward;
function flip_bit(number: integer; position: integer): integer; forward;
function get_bit(number: integer; position: integer): integer; forward;
function pow2(exp: integer): integer;
var
  pow2_result_: integer;
  pow2_i: integer;
begin
  pow2_result_ := 1;
  pow2_i := 0;
  while pow2_i < exp do begin
  pow2_result_ := pow2_result_ * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_result_);
end;
function is_bit_set(number: integer; position: integer): boolean;
var
  is_bit_set_shifted: integer;
  is_bit_set_remainder: integer;
begin
  is_bit_set_shifted := number div pow2(position);
  is_bit_set_remainder := is_bit_set_shifted mod 2;
  exit(is_bit_set_remainder = 1);
end;
function set_bit(number: integer; position: integer): integer;
begin
  if is_bit_set(number, position) then begin
  exit(number);
end;
  exit(number + pow2(position));
end;
function clear_bit(number: integer; position: integer): integer;
begin
  if is_bit_set(number, position) then begin
  exit(number - pow2(position));
end;
  exit(number);
end;
function flip_bit(number: integer; position: integer): integer;
begin
  if is_bit_set(number, position) then begin
  exit(number - pow2(position));
end;
  exit(number + pow2(position));
end;
function get_bit(number: integer; position: integer): integer;
begin
  if is_bit_set(number, position) then begin
  exit(1);
end;
  exit(0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(set_bit(13, 1)));
  writeln(IntToStr(clear_bit(18, 1)));
  writeln(IntToStr(flip_bit(5, 1)));
  writeln(LowerCase(BoolToStr(is_bit_set(10, 3), true)));
  writeln(IntToStr(get_bit(10, 1)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
