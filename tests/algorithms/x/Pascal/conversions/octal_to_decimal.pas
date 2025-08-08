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
  s: string;
  ch: string;
  oct_string: string;
  msg: string;
procedure panic_(msg: string); forward;
function trim_spaces(s: string): string; forward;
function char_to_digit(ch: string): integer; forward;
function oct_to_decimal(oct_string: string): integer; forward;
procedure main(); forward;
procedure panic_(msg: string);
begin
  writeln(msg);
end;
function trim_spaces(s: string): string;
var
  trim_spaces_start: integer;
  trim_spaces_end_: integer;
begin
  trim_spaces_start := 0;
  trim_spaces_end_ := Length(s) - 1;
  while (trim_spaces_start <= trim_spaces_end_) and (copy(s, trim_spaces_start+1, (trim_spaces_start + 1 - (trim_spaces_start))) = ' ') do begin
  trim_spaces_start := trim_spaces_start + 1;
end;
  while (trim_spaces_end_ >= trim_spaces_start) and (copy(s, trim_spaces_end_+1, (trim_spaces_end_ + 1 - (trim_spaces_end_))) = ' ') do begin
  trim_spaces_end_ := trim_spaces_end_ - 1;
end;
  if trim_spaces_start > trim_spaces_end_ then begin
  exit('');
end;
  exit(copy(s, trim_spaces_start+1, (trim_spaces_end_ + 1 - (trim_spaces_start))));
end;
function char_to_digit(ch: string): integer;
begin
  if ch = '0' then begin
  exit(0);
end;
  if ch = '1' then begin
  exit(1);
end;
  if ch = '2' then begin
  exit(2);
end;
  if ch = '3' then begin
  exit(3);
end;
  if ch = '4' then begin
  exit(4);
end;
  if ch = '5' then begin
  exit(5);
end;
  if ch = '6' then begin
  exit(6);
end;
  if ch = '7' then begin
  exit(7);
end;
  panic_('Non-octal value was passed to the function');
  exit(0);
end;
function oct_to_decimal(oct_string: string): integer;
var
  oct_to_decimal_s: string;
  oct_to_decimal_is_negative: boolean;
  oct_to_decimal_decimal_number: integer;
  oct_to_decimal_i: integer;
  oct_to_decimal_ch: string;
  oct_to_decimal_digit: integer;
begin
  oct_to_decimal_s := trim_spaces(oct_string);
  if Length(oct_to_decimal_s) = 0 then begin
  panic_('Empty string was passed to the function');
  exit(0);
end;
  oct_to_decimal_is_negative := false;
  if copy(oct_to_decimal_s, 0+1, (1 - (0))) = '-' then begin
  oct_to_decimal_is_negative := true;
  oct_to_decimal_s := copy(oct_to_decimal_s, 1+1, (Length(oct_to_decimal_s) - (1)));
end;
  if Length(oct_to_decimal_s) = 0 then begin
  panic_('Non-octal value was passed to the function');
  exit(0);
end;
  oct_to_decimal_decimal_number := 0;
  oct_to_decimal_i := 0;
  while oct_to_decimal_i < Length(oct_to_decimal_s) do begin
  oct_to_decimal_ch := copy(oct_to_decimal_s, oct_to_decimal_i+1, (oct_to_decimal_i + 1 - (oct_to_decimal_i)));
  oct_to_decimal_digit := char_to_digit(oct_to_decimal_ch);
  oct_to_decimal_decimal_number := (8 * oct_to_decimal_decimal_number) + oct_to_decimal_digit;
  oct_to_decimal_i := oct_to_decimal_i + 1;
end;
  if oct_to_decimal_is_negative then begin
  oct_to_decimal_decimal_number := -oct_to_decimal_decimal_number;
end;
  exit(oct_to_decimal_decimal_number);
end;
procedure main();
begin
  writeln(IntToStr(oct_to_decimal('1')));
  writeln(IntToStr(oct_to_decimal('-1')));
  writeln(IntToStr(oct_to_decimal('12')));
  writeln(IntToStr(oct_to_decimal(' 12   ')));
  writeln(IntToStr(oct_to_decimal('-45')));
  writeln(IntToStr(oct_to_decimal('0')));
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
