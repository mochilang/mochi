{$mode objfpc}
program Main;
uses SysUtils, Math;
type IntArray = array of integer;
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
  price: array of integer;
  spans: IntArray;
  arr: IntArray;
function calculation_span(price: IntArray): IntArray; forward;
procedure print_array(arr: IntArray); forward;
function calculation_span(price: IntArray): IntArray;
var
  calculation_span_n: integer;
  calculation_span_st: array of integer;
  calculation_span_span: array of integer;
  calculation_span_i: integer;
  calculation_span_s: integer;
begin
  calculation_span_n := Length(price);
  calculation_span_st := IntArray([]);
  calculation_span_span := IntArray([]);
  calculation_span_st := concat(calculation_span_st, IntArray([0]));
  calculation_span_span := concat(calculation_span_span, IntArray([1]));
  for calculation_span_i := 1 to (calculation_span_n - 1) do begin
  while (Length(calculation_span_st) > 0) and (price[calculation_span_st[Length(calculation_span_st) - 1]] <= price[calculation_span_i]) do begin
  calculation_span_st := copy(calculation_span_st, 0, (Length(calculation_span_st) - 1 - (0)));
end;
  if Length(calculation_span_st) <= 0 then begin
  calculation_span_s := calculation_span_i + 1;
end else begin
  calculation_span_s := calculation_span_i - calculation_span_st[Length(calculation_span_st) - 1];
end;
  calculation_span_span := concat(calculation_span_span, IntArray([calculation_span_s]));
  calculation_span_st := concat(calculation_span_st, IntArray([calculation_span_i]));
end;
  exit(calculation_span_span);
end;
procedure print_array(arr: IntArray);
var
  print_array_i: integer;
begin
  for print_array_i := 0 to (Length(arr) - 1) do begin
  writeln(arr[print_array_i]);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  price := [10, 4, 5, 90, 120, 80];
  spans := calculation_span(price);
  print_array(spans);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
