{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  elements_to_choose: integer;
  max_set_length: integer;
  total_elements: integer;
function binomial_coefficient(total_elements: integer; elements_to_choose: integer): integer; forward;
function bell_numbers(max_set_length: integer): IntArray; forward;
procedure main(); forward;
function binomial_coefficient(total_elements: integer; elements_to_choose: integer): integer;
var
  binomial_coefficient_k: integer;
  binomial_coefficient_coefficient: integer;
  binomial_coefficient_i: integer;
begin
  if (elements_to_choose = 0) or (elements_to_choose = total_elements) then begin
  exit(1);
end;
  binomial_coefficient_k := elements_to_choose;
  if binomial_coefficient_k > (total_elements - binomial_coefficient_k) then begin
  binomial_coefficient_k := total_elements - binomial_coefficient_k;
end;
  binomial_coefficient_coefficient := 1;
  binomial_coefficient_i := 0;
  while binomial_coefficient_i < binomial_coefficient_k do begin
  binomial_coefficient_coefficient := binomial_coefficient_coefficient * (total_elements - binomial_coefficient_i);
  binomial_coefficient_coefficient := binomial_coefficient_coefficient div (binomial_coefficient_i + 1);
  binomial_coefficient_i := binomial_coefficient_i + 1;
end;
  exit(binomial_coefficient_coefficient);
end;
function bell_numbers(max_set_length: integer): IntArray;
var
  bell_numbers_bell: array of integer;
  bell_numbers_i: integer;
  bell_numbers_j: integer;
begin
  if max_set_length < 0 then begin
  panic('max_set_length must be non-negative');
end;
  bell_numbers_bell := [];
  bell_numbers_i := 0;
  while bell_numbers_i <= max_set_length do begin
  bell_numbers_bell := concat(bell_numbers_bell, IntArray([0]));
  bell_numbers_i := bell_numbers_i + 1;
end;
  bell_numbers_bell[0] := 1;
  bell_numbers_i := 1;
  while bell_numbers_i <= max_set_length do begin
  bell_numbers_j := 0;
  while bell_numbers_j < bell_numbers_i do begin
  bell_numbers_bell[bell_numbers_i] := bell_numbers_bell[bell_numbers_i] + (binomial_coefficient(bell_numbers_i - 1, bell_numbers_j) * bell_numbers_bell[bell_numbers_j]);
  bell_numbers_j := bell_numbers_j + 1;
end;
  bell_numbers_i := bell_numbers_i + 1;
end;
  exit(bell_numbers_bell);
end;
procedure main();
begin
  writeln(list_int_to_str(bell_numbers(5)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
