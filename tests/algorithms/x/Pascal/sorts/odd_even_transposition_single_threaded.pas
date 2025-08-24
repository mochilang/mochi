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
procedure json(x: int64);
begin
  writeln(x);
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
function odd_even_transposition(odd_even_transposition_arr: RealArray): RealArray; forward;
function odd_even_transposition(odd_even_transposition_arr: RealArray): RealArray;
var
  odd_even_transposition_n: integer;
  odd_even_transposition_pass: int64;
  odd_even_transposition_i: int64;
  odd_even_transposition_tmp: real;
begin
  odd_even_transposition_n := Length(odd_even_transposition_arr);
  odd_even_transposition_pass := 0;
  while odd_even_transposition_pass < odd_even_transposition_n do begin
  odd_even_transposition_i := odd_even_transposition_pass mod 2;
  while odd_even_transposition_i < (odd_even_transposition_n - 1) do begin
  if odd_even_transposition_arr[odd_even_transposition_i + 1] < odd_even_transposition_arr[odd_even_transposition_i] then begin
  odd_even_transposition_tmp := odd_even_transposition_arr[odd_even_transposition_i];
  odd_even_transposition_arr[odd_even_transposition_i] := odd_even_transposition_arr[odd_even_transposition_i + 1];
  odd_even_transposition_arr[odd_even_transposition_i + 1] := odd_even_transposition_tmp;
end;
  odd_even_transposition_i := odd_even_transposition_i + 2;
end;
  odd_even_transposition_pass := odd_even_transposition_pass + 1;
end;
  exit(odd_even_transposition_arr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_real_to_str(odd_even_transposition([5, 4, 3, 2, 1])));
  writeln(list_real_to_str(odd_even_transposition([13, 11, 18, 0, -1])));
  writeln(list_real_to_str(odd_even_transposition([-0.1, 1.1, 0.1, -2.9])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
