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
  msg: string;
  hex_num: string;
procedure panic_(msg: string); forward;
function trim_spaces(s: string): string; forward;
function hex_digit_value(ch: string): integer; forward;
function hex_to_bin(hex_num: string): integer; forward;
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
  trim_spaces_end_ := Length(s);
  while (trim_spaces_start < trim_spaces_end_) and (copy(s, trim_spaces_start+1, (trim_spaces_start + 1 - (trim_spaces_start))) = ' ') do begin
  trim_spaces_start := trim_spaces_start + 1;
end;
  while (trim_spaces_end_ > trim_spaces_start) and (copy(s, trim_spaces_end_ - 1+1, (trim_spaces_end_ - (trim_spaces_end_ - 1))) = ' ') do begin
  trim_spaces_end_ := trim_spaces_end_ - 1;
end;
  exit(copy(s, trim_spaces_start+1, (trim_spaces_end_ - (trim_spaces_start))));
end;
function hex_digit_value(ch: string): integer;
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
  if ch = '8' then begin
  exit(8);
end;
  if ch = '9' then begin
  exit(9);
end;
  if (ch = 'a') or (ch = 'A') then begin
  exit(10);
end;
  if (ch = 'b') or (ch = 'B') then begin
  exit(11);
end;
  if (ch = 'c') or (ch = 'C') then begin
  exit(12);
end;
  if (ch = 'd') or (ch = 'D') then begin
  exit(13);
end;
  if (ch = 'e') or (ch = 'E') then begin
  exit(14);
end;
  if (ch = 'f') or (ch = 'F') then begin
  exit(15);
end;
  panic_('Invalid value was passed to the function');
end;
function hex_to_bin(hex_num: string): integer;
var
  hex_to_bin_trimmed: string;
  hex_to_bin_s: string;
  hex_to_bin_is_negative: boolean;
  hex_to_bin_int_num: integer;
  hex_to_bin_i: integer;
  hex_to_bin_ch: string;
  hex_to_bin_val: integer;
  hex_to_bin_bin_str: string;
  hex_to_bin_n: integer;
  hex_to_bin_result_: integer;
begin
  hex_to_bin_trimmed := trim_spaces(hex_num);
  if Length(hex_to_bin_trimmed) = 0 then begin
  panic_('No value was passed to the function');
end;
  hex_to_bin_s := hex_to_bin_trimmed;
  hex_to_bin_is_negative := false;
  if copy(hex_to_bin_s, 0+1, (1 - (0))) = '-' then begin
  hex_to_bin_is_negative := true;
  hex_to_bin_s := copy(hex_to_bin_s, 1+1, (Length(hex_to_bin_s) - (1)));
end;
  hex_to_bin_int_num := 0;
  hex_to_bin_i := 0;
  while hex_to_bin_i < Length(hex_to_bin_s) do begin
  hex_to_bin_ch := copy(hex_to_bin_s, hex_to_bin_i+1, (hex_to_bin_i + 1 - (hex_to_bin_i)));
  hex_to_bin_val := hex_digit_value(hex_to_bin_ch);
  hex_to_bin_int_num := (hex_to_bin_int_num * 16) + hex_to_bin_val;
  hex_to_bin_i := hex_to_bin_i + 1;
end;
  hex_to_bin_bin_str := '';
  hex_to_bin_n := hex_to_bin_int_num;
  if hex_to_bin_n = 0 then begin
  hex_to_bin_bin_str := '0';
end;
  while hex_to_bin_n > 0 do begin
  hex_to_bin_bin_str := IntToStr(hex_to_bin_n mod 2) + hex_to_bin_bin_str;
  hex_to_bin_n := hex_to_bin_n div 2;
end;
  hex_to_bin_result_ := StrToInt(hex_to_bin_bin_str);
  if hex_to_bin_is_negative then begin
  hex_to_bin_result_ := -hex_to_bin_result_;
end;
  exit(hex_to_bin_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(hex_to_bin('AC')));
  writeln(IntToStr(hex_to_bin('9A4')));
  writeln(IntToStr(hex_to_bin('   12f   ')));
  writeln(IntToStr(hex_to_bin('FfFf')));
  writeln(IntToStr(hex_to_bin('-fFfF')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
