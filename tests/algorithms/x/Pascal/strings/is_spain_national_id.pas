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
  DIGITS: string;
  UPPER: string;
  LOWER: string;
  LOOKUP_LETTERS: string;
  ERROR_MSG: string;
function to_upper(to_upper_s: string): string; forward;
function is_digit(is_digit_ch: string): boolean; forward;
function clean_id(clean_id_spanish_id: string): string; forward;
function is_spain_national_id(is_spain_national_id_spanish_id: string): boolean; forward;
procedure main(); forward;
function to_upper(to_upper_s: string): string;
var
  to_upper_res: string;
  to_upper_i: integer;
  to_upper_ch: string;
  to_upper_j: integer;
  to_upper_converted: string;
begin
  to_upper_res := '';
  to_upper_i := 0;
  while to_upper_i < Length(to_upper_s) do begin
  to_upper_ch := to_upper_s[to_upper_i+1];
  to_upper_j := 0;
  to_upper_converted := to_upper_ch;
  while to_upper_j < Length(LOWER) do begin
  if LOWER[to_upper_j+1] = to_upper_ch then begin
  to_upper_converted := UPPER[to_upper_j+1];
  break;
end;
  to_upper_j := to_upper_j + 1;
end;
  to_upper_res := to_upper_res + to_upper_converted;
  to_upper_i := to_upper_i + 1;
end;
  exit(to_upper_res);
end;
function is_digit(is_digit_ch: string): boolean;
var
  is_digit_i: integer;
begin
  is_digit_i := 0;
  while is_digit_i < Length(DIGITS) do begin
  if DIGITS[is_digit_i+1] = is_digit_ch then begin
  exit(true);
end;
  is_digit_i := is_digit_i + 1;
end;
  exit(false);
end;
function clean_id(clean_id_spanish_id: string): string;
var
  clean_id_upper_id: string;
  clean_id_cleaned: string;
  clean_id_i: integer;
  clean_id_ch: string;
begin
  clean_id_upper_id := to_upper(clean_id_spanish_id);
  clean_id_cleaned := '';
  clean_id_i := 0;
  while clean_id_i < Length(clean_id_upper_id) do begin
  clean_id_ch := clean_id_upper_id[clean_id_i+1];
  if clean_id_ch <> '-' then begin
  clean_id_cleaned := clean_id_cleaned + clean_id_ch;
end;
  clean_id_i := clean_id_i + 1;
end;
  exit(clean_id_cleaned);
end;
function is_spain_national_id(is_spain_national_id_spanish_id: string): boolean;
var
  is_spain_national_id_sid: string;
  is_spain_national_id_i: integer;
  is_spain_national_id_number: int64;
  is_spain_national_id_letter: string;
  is_spain_national_id_expected: string;
begin
  is_spain_national_id_sid := clean_id(is_spain_national_id_spanish_id);
  if Length(is_spain_national_id_sid) <> 9 then begin
  panic(ERROR_MSG);
end;
  is_spain_national_id_i := 0;
  while is_spain_national_id_i < 8 do begin
  if not is_digit(is_spain_national_id_sid[is_spain_national_id_i+1]) then begin
  panic(ERROR_MSG);
end;
  is_spain_national_id_i := is_spain_national_id_i + 1;
end;
  is_spain_national_id_number := StrToInt(copy(is_spain_national_id_sid, 1, 8));
  is_spain_national_id_letter := is_spain_national_id_sid[8+1];
  if is_digit(is_spain_national_id_letter) then begin
  panic(ERROR_MSG);
end;
  is_spain_national_id_expected := LOOKUP_LETTERS[is_spain_national_id_number mod 23+1];
  exit(is_spain_national_id_letter = is_spain_national_id_expected);
end;
procedure main();
begin
  writeln(Ord(is_spain_national_id('12345678Z')));
  writeln(Ord(is_spain_national_id('12345678z')));
  writeln(Ord(is_spain_national_id('12345678x')));
  writeln(Ord(is_spain_national_id('12345678I')));
  writeln(Ord(is_spain_national_id('12345678-Z')));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  DIGITS := '0123456789';
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  LOOKUP_LETTERS := 'TRWAGMYFPDXBNJZSQVHLCKE';
  ERROR_MSG := 'Input must be a string of 8 numbers plus letter';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
