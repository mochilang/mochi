{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
function list_int_to_str(xs: array of int64): string;
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
procedure stooge(stooge_arr: IntArray; stooge_i: int64; stooge_h: int64); forward;
function stooge_sort(stooge_sort_arr: IntArray): IntArray; forward;
procedure stooge(stooge_arr: IntArray; stooge_i: int64; stooge_h: int64);
var
  stooge_tmp: int64;
  stooge_t: integer;
begin
  if stooge_i >= stooge_h then begin
  exit();
end;
  if stooge_arr[stooge_i] > stooge_arr[stooge_h] then begin
  stooge_tmp := stooge_arr[stooge_i];
  stooge_arr[stooge_i] := stooge_arr[stooge_h];
  stooge_arr[stooge_h] := stooge_tmp;
end;
  if ((stooge_h - stooge_i) + 1) > 2 then begin
  stooge_t := Trunc(((stooge_h - stooge_i) + 1) div 3);
  stooge(stooge_arr, stooge_i, stooge_h - stooge_t);
  stooge(stooge_arr, stooge_i + stooge_t, stooge_h);
  stooge(stooge_arr, stooge_i, stooge_h - stooge_t);
end;
end;
function stooge_sort(stooge_sort_arr: IntArray): IntArray;
begin
  stooge(stooge_sort_arr, 0, Length(stooge_sort_arr) - 1);
  exit(stooge_sort_arr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(stooge_sort([18, 0, -7, -1, 2, 2])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
