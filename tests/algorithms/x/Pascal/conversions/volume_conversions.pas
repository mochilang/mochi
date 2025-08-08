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
  units: array of string;
  from_factors: array of real;
  to_factors: array of real;
  name: string;
  value: real;
  to_type: string;
  from_type: string;
function supported_values(): string; forward;
function find_index(name: string): integer; forward;
function get_from_factor(name: string): real; forward;
function get_to_factor(name: string): real; forward;
function volume_conversion(value: real; from_type: string; to_type: string): real; forward;
function supported_values(): string;
var
  supported_values_result_: string;
  supported_values_i: integer;
begin
  supported_values_result_ := units[0];
  supported_values_i := 1;
  while supported_values_i < Length(units) do begin
  supported_values_result_ := (supported_values_result_ + ', ') + units[supported_values_i];
  supported_values_i := supported_values_i + 1;
end;
  exit(supported_values_result_);
end;
function find_index(name: string): integer;
var
  find_index_i: integer;
begin
  find_index_i := 0;
  while find_index_i < Length(units) do begin
  if units[find_index_i] = name then begin
  exit(find_index_i);
end;
  find_index_i := find_index_i + 1;
end;
  exit(-1);
end;
function get_from_factor(name: string): real;
var
  get_from_factor_idx: integer;
begin
  get_from_factor_idx := find_index(name);
  if get_from_factor_idx < 0 then begin
  panic((('Invalid ''from_type'' value: ''' + name) + ''' Supported values are: ') + supported_values());
end;
  exit(from_factors[get_from_factor_idx]);
end;
function get_to_factor(name: string): real;
var
  get_to_factor_idx: integer;
begin
  get_to_factor_idx := find_index(name);
  if get_to_factor_idx < 0 then begin
  panic((('Invalid ''to_type'' value: ''' + name) + ''' Supported values are: ') + supported_values());
end;
  exit(to_factors[get_to_factor_idx]);
end;
function volume_conversion(value: real; from_type: string; to_type: string): real;
var
  volume_conversion_from_factor: real;
  volume_conversion_to_factor: real;
begin
  volume_conversion_from_factor := get_from_factor(from_type);
  volume_conversion_to_factor := get_to_factor(to_type);
  exit((value * volume_conversion_from_factor) * volume_conversion_to_factor);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  units := ['cubic meter', 'litre', 'kilolitre', 'gallon', 'cubic yard', 'cubic foot', 'cup'];
  from_factors := [1, 0.001, 1, 0.00454, 0.76455, 0.028, 0.000236588];
  to_factors := [1, 1000, 1, 264.172, 1.30795, 35.3147, 4226.75];
  writeln(FloatToStr(volume_conversion(4, 'cubic meter', 'litre')));
  writeln(FloatToStr(volume_conversion(1, 'litre', 'gallon')));
  writeln(FloatToStr(volume_conversion(1, 'kilolitre', 'cubic meter')));
  writeln(FloatToStr(volume_conversion(3, 'gallon', 'cubic yard')));
  writeln(FloatToStr(volume_conversion(2, 'cubic yard', 'litre')));
  writeln(FloatToStr(volume_conversion(4, 'cubic foot', 'cup')));
  writeln(FloatToStr(volume_conversion(1, 'cup', 'kilolitre')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
