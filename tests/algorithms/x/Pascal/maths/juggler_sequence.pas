{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x: real;
  n: integer;
function to_float(x: integer): real; forward;
function sqrt(x: real): real; forward;
function floor(x: real): integer; forward;
function juggler_sequence(n: integer): IntArray; forward;
function to_float(x: integer): real;
begin
  exit(x * 1);
end;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_guess := x;
  sqrt_i := 0;
  while sqrt_i < 10 do begin
  sqrt_guess := (sqrt_guess + (x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function floor(x: real): integer;
var
  floor_n: integer;
  floor_y: real;
begin
  floor_n := 0;
  floor_y := x;
  while floor_y >= 1 do begin
  floor_y := floor_y - 1;
  floor_n := floor_n + 1;
end;
  exit(floor_n);
end;
function juggler_sequence(n: integer): IntArray;
var
  juggler_sequence_seq: array of integer;
  juggler_sequence_current: integer;
  juggler_sequence_r: real;
begin
  if n < 1 then begin
  panic('number must be a positive integer');
end;
  juggler_sequence_seq := [n];
  juggler_sequence_current := n;
  while juggler_sequence_current <> 1 do begin
  if (juggler_sequence_current mod 2) = 0 then begin
  juggler_sequence_current := Floor(sqrt(to_float(juggler_sequence_current)));
end else begin
  juggler_sequence_r := sqrt(to_float(juggler_sequence_current));
  juggler_sequence_current := Floor((juggler_sequence_r * juggler_sequence_r) * juggler_sequence_r);
end;
  juggler_sequence_seq := concat(juggler_sequence_seq, IntArray([juggler_sequence_current]));
end;
  exit(juggler_sequence_seq);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(juggler_sequence(3)));
  writeln(list_int_to_str(juggler_sequence(10)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
