{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure show_list_real(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  common_ratio_r: real;
  nth_term: real;
  start_term_a: real;
function geometric_series(nth_term: real; start_term_a: real; common_ratio_r: real): RealArray; forward;
function geometric_series(nth_term: real; start_term_a: real; common_ratio_r: real): RealArray;
var
  geometric_series_n: integer;
  geometric_series_series: array of real;
  geometric_series_current: real;
  geometric_series_i: integer;
begin
  geometric_series_n := Trunc(nth_term);
  if ((geometric_series_n <= 0) or (start_term_a = 0)) or (common_ratio_r = 0) then begin
  exit([]);
end;
  geometric_series_series := [];
  geometric_series_current := start_term_a;
  geometric_series_i := 0;
  while geometric_series_i < geometric_series_n do begin
  geometric_series_series := concat(geometric_series_series, [geometric_series_current]);
  geometric_series_current := geometric_series_current * common_ratio_r;
  geometric_series_i := geometric_series_i + 1;
end;
  exit(geometric_series_series);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list_real(geometric_series(4, 2, 2));
  show_list_real(geometric_series(4, 2, -2));
  show_list_real(geometric_series(4, -2, 2));
  show_list_real(geometric_series(-4, 2, 2));
  show_list_real(geometric_series(0, 100, 500));
  show_list_real(geometric_series(1, 1, 1));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
