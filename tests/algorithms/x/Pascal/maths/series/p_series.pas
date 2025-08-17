{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  base: integer;
  exp_: integer;
  nth_term: integer;
  power: integer;
function pow_string(base: integer; pow_string_exp_: integer): string; forward;
function p_series(nth_term: integer; power: integer): StrArray; forward;
function pow_string(base: integer; pow_string_exp_: integer): string;
var
  pow_string_res: integer;
  pow_string_i: integer;
  pow_string_e: integer;
  pow_string_res_7: real;
  pow_string_b: real;
  pow_string_i_9: integer;
  pow_string_value: real;
begin
  if exp_ >= 0 then begin
  pow_string_res := 1;
  pow_string_i := 0;
  while pow_string_i < exp_ do begin
  pow_string_res := pow_string_res * base;
  pow_string_i := pow_string_i + 1;
end;
  exit(IntToStr(pow_string_res));
end;
  pow_string_e := -exp_;
  pow_string_res_7 := 1;
  pow_string_b := base * 1;
  pow_string_i_9 := 0;
  while pow_string_i_9 < pow_string_e do begin
  pow_string_res_7 := pow_string_res_7 * pow_string_b;
  pow_string_i_9 := pow_string_i_9 + 1;
end;
  pow_string_value := 1 / pow_string_res_7;
  exit(FloatToStr(pow_string_value));
end;
function p_series(nth_term: integer; power: integer): StrArray;
var
  p_series_series: array of string;
  p_series_i: integer;
begin
  p_series_series := [];
  if nth_term <= 0 then begin
  exit(p_series_series);
end;
  p_series_i := 1;
  while p_series_i <= nth_term do begin
  if p_series_i = 1 then begin
  p_series_series := concat(p_series_series, StrArray(['1']));
end else begin
  p_series_series := concat(p_series_series, StrArray(['1 / ' + pow_string(p_series_i, power)]));
end;
  p_series_i := p_series_i + 1;
end;
  exit(p_series_series);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_to_str(p_series(5, 2)));
  writeln(list_to_str(p_series(-5, 2)));
  writeln(list_to_str(p_series(5, -2)));
  writeln(list_to_str(p_series(0, 0)));
  writeln(list_to_str(p_series(1, 1)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
