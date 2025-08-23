{$mode objfpc}{$modeswitch nestedprocvars}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function parse_int(parse_int_s: string): integer; forward;
function is_polish_national_id(is_polish_national_id_id: string): boolean; forward;
function parse_int(parse_int_s: string): integer;
var
  parse_int_value: integer;
  parse_int_i: integer;
  parse_int_c: string;
begin
  parse_int_value := 0;
  parse_int_i := 0;
  while parse_int_i < Length(parse_int_s) do begin
  parse_int_c := parse_int_s[parse_int_i+1];
  parse_int_value := (parse_int_value * 10) + StrToInt(parse_int_c);
  parse_int_i := parse_int_i + 1;
end;
  exit(parse_int_value);
end;
function is_polish_national_id(is_polish_national_id_id: string): boolean;
var
  is_polish_national_id_input_int: integer;
  is_polish_national_id_month: integer;
  is_polish_national_id_day: integer;
  is_polish_national_id_multipliers: array of integer;
  is_polish_national_id_subtotal: integer;
  is_polish_national_id_i: integer;
  is_polish_national_id_digit: integer;
  is_polish_national_id_checksum: integer;
begin
  if Length(is_polish_national_id_id) = 0 then begin
  exit(false);
end;
  if copy(is_polish_national_id_id, 1, 1) = '-' then begin
  exit(false);
end;
  is_polish_national_id_input_int := parse_int(is_polish_national_id_id);
  if (is_polish_national_id_input_int < 10100000) or (is_polish_national_id_input_int > 99923199999) then begin
  exit(false);
end;
  is_polish_national_id_month := parse_int(copy(is_polish_national_id_id, 3, 2));
  if not ((((((is_polish_national_id_month >= 1) and (is_polish_national_id_month <= 12)) or ((is_polish_national_id_month >= 21) and (is_polish_national_id_month <= 32))) or ((is_polish_national_id_month >= 41) and (is_polish_national_id_month <= 52))) or ((is_polish_national_id_month >= 61) and (is_polish_national_id_month <= 72))) or ((is_polish_national_id_month >= 81) and (is_polish_national_id_month <= 92))) then begin
  exit(false);
end;
  is_polish_national_id_day := parse_int(copy(is_polish_national_id_id, 5, 2));
  if (is_polish_national_id_day < 1) or (is_polish_national_id_day > 31) then begin
  exit(false);
end;
  is_polish_national_id_multipliers := [1, 3, 7, 9, 1, 3, 7, 9, 1, 3];
  is_polish_national_id_subtotal := 0;
  is_polish_national_id_i := 0;
  while is_polish_national_id_i < Length(is_polish_national_id_multipliers) do begin
  is_polish_national_id_digit := parse_int(copy(is_polish_national_id_id, is_polish_national_id_i+1, (is_polish_national_id_i + 1 - (is_polish_national_id_i))));
  is_polish_national_id_subtotal := is_polish_national_id_subtotal + ((is_polish_national_id_digit * is_polish_national_id_multipliers[is_polish_national_id_i]) mod 10);
  is_polish_national_id_i := is_polish_national_id_i + 1;
end;
  is_polish_national_id_checksum := 10 - (is_polish_national_id_subtotal mod 10);
  exit(is_polish_national_id_checksum = (is_polish_national_id_input_int mod 10));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_polish_national_id('02070803628'), true)));
  writeln(LowerCase(BoolToStr(is_polish_national_id('02150803629'), true)));
  writeln(LowerCase(BoolToStr(is_polish_national_id('02075503622'), true)));
  writeln(LowerCase(BoolToStr(is_polish_national_id('-99012212349'), true)));
  writeln(LowerCase(BoolToStr(is_polish_national_id('990122123499999'), true)));
  writeln(LowerCase(BoolToStr(is_polish_national_id('02070803621'), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
