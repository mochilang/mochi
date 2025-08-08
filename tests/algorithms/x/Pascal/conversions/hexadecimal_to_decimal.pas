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
  hex_string: string;
  s: string;
  c: string;
function strip(s: string): string; forward;
function hex_digit_value(c: string): integer; forward;
function hex_to_decimal(hex_string: string): integer; forward;
procedure main(); forward;
function strip(s: string): string;
var
  strip_start: integer;
  strip_end_: integer;
begin
  strip_start := 0;
  strip_end_ := Length(s);
  while (strip_start < strip_end_) and (copy(s, strip_start+1, (strip_start + 1 - (strip_start))) = ' ') do begin
  strip_start := strip_start + 1;
end;
  while (strip_end_ > strip_start) and (copy(s, strip_end_ - 1+1, (strip_end_ - (strip_end_ - 1))) = ' ') do begin
  strip_end_ := strip_end_ - 1;
end;
  exit(copy(s, strip_start+1, (strip_end_ - (strip_start))));
end;
function hex_digit_value(c: string): integer;
begin
  if c = '0' then begin
  exit(0);
end;
  if c = '1' then begin
  exit(1);
end;
  if c = '2' then begin
  exit(2);
end;
  if c = '3' then begin
  exit(3);
end;
  if c = '4' then begin
  exit(4);
end;
  if c = '5' then begin
  exit(5);
end;
  if c = '6' then begin
  exit(6);
end;
  if c = '7' then begin
  exit(7);
end;
  if c = '8' then begin
  exit(8);
end;
  if c = '9' then begin
  exit(9);
end;
  if (c = 'a') or (c = 'A') then begin
  exit(10);
end;
  if (c = 'b') or (c = 'B') then begin
  exit(11);
end;
  if (c = 'c') or (c = 'C') then begin
  exit(12);
end;
  if (c = 'd') or (c = 'D') then begin
  exit(13);
end;
  if (c = 'e') or (c = 'E') then begin
  exit(14);
end;
  if (c = 'f') or (c = 'F') then begin
  exit(15);
end;
  writeln('Non-hexadecimal value was passed to the function');
  exit(0);
end;
function hex_to_decimal(hex_string: string): integer;
var
  hex_to_decimal_s: string;
  hex_to_decimal_is_negative: boolean;
  hex_to_decimal_decimal_number: integer;
  hex_to_decimal_i: integer;
  hex_to_decimal_c: string;
  hex_to_decimal_value: integer;
begin
  hex_to_decimal_s := strip(hex_string);
  if Length(hex_to_decimal_s) = 0 then begin
  writeln('Empty string was passed to the function');
  exit(0);
end;
  hex_to_decimal_is_negative := false;
  if copy(hex_to_decimal_s, 0+1, (1 - (0))) = '-' then begin
  hex_to_decimal_is_negative := true;
  hex_to_decimal_s := copy(hex_to_decimal_s, 1+1, (Length(hex_to_decimal_s) - (1)));
end;
  hex_to_decimal_decimal_number := 0;
  hex_to_decimal_i := 0;
  while hex_to_decimal_i < Length(hex_to_decimal_s) do begin
  hex_to_decimal_c := copy(hex_to_decimal_s, hex_to_decimal_i+1, (hex_to_decimal_i + 1 - (hex_to_decimal_i)));
  hex_to_decimal_value := hex_digit_value(hex_to_decimal_c);
  hex_to_decimal_decimal_number := (16 * hex_to_decimal_decimal_number) + hex_to_decimal_value;
  hex_to_decimal_i := hex_to_decimal_i + 1;
end;
  if hex_to_decimal_is_negative then begin
  exit(-hex_to_decimal_decimal_number);
end;
  exit(hex_to_decimal_decimal_number);
end;
procedure main();
begin
  writeln(IntToStr(hex_to_decimal('a')));
  writeln(IntToStr(hex_to_decimal('12f')));
  writeln(IntToStr(hex_to_decimal('   12f   ')));
  writeln(IntToStr(hex_to_decimal('FfFf')));
  writeln(IntToStr(hex_to_decimal('-Ff')));
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
