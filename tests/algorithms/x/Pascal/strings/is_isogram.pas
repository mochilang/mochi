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
function index_of(index_of_s: string; index_of_ch: string): integer; forward;
function ord_(ord__ch: string): integer; forward;
function chr(chr_n: integer): string; forward;
function to_lower_char(to_lower_char_c: string): string; forward;
function is_alpha(is_alpha_c: string): boolean; forward;
function is_isogram(is_isogram_s: string): boolean; forward;
function index_of(index_of_s: string; index_of_ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(index_of_s) do begin
  if index_of_s[index_of_i+1] = index_of_ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function ord_(ord__ch: string): integer;
var
  ord__upper: string;
  ord__lower: string;
  ord__idx: integer;
begin
  ord__upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ord__lower := 'abcdefghijklmnopqrstuvwxyz';
  ord__idx := index_of(ord__upper, ord__ch);
  if ord__idx >= 0 then begin
  exit(65 + ord__idx);
end;
  ord__idx := index_of(ord__lower, ord__ch);
  if ord__idx >= 0 then begin
  exit(97 + ord__idx);
end;
  exit(-1);
end;
function chr(chr_n: integer): string;
var
  chr_upper: string;
  chr_lower: string;
begin
  chr_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  chr_lower := 'abcdefghijklmnopqrstuvwxyz';
  if (chr_n >= 65) and (chr_n < 91) then begin
  exit(copy(chr_upper, chr_n - 65+1, (chr_n - 64 - (chr_n - 65))));
end;
  if (chr_n >= 97) and (chr_n < 123) then begin
  exit(copy(chr_lower, chr_n - 97+1, (chr_n - 96 - (chr_n - 97))));
end;
  exit('?');
end;
function to_lower_char(to_lower_char_c: string): string;
var
  to_lower_char_code: integer;
begin
  to_lower_char_code := ord_(to_lower_char_c);
  if (to_lower_char_code >= 65) and (to_lower_char_code <= 90) then begin
  exit(chr(to_lower_char_code + 32));
end;
  exit(to_lower_char_c);
end;
function is_alpha(is_alpha_c: string): boolean;
var
  is_alpha_code: integer;
begin
  is_alpha_code := ord_(is_alpha_c);
  exit(((is_alpha_code >= 65) and (is_alpha_code <= 90)) or ((is_alpha_code >= 97) and (is_alpha_code <= 122)));
end;
function is_isogram(is_isogram_s: string): boolean;
var
  is_isogram_seen: string;
  is_isogram_i: integer;
  is_isogram_ch: string;
  is_isogram_lower: string;
begin
  is_isogram_seen := '';
  is_isogram_i := 0;
  while is_isogram_i < Length(is_isogram_s) do begin
  is_isogram_ch := is_isogram_s[is_isogram_i+1];
  if not is_alpha(is_isogram_ch) then begin
  panic('String must only contain alphabetic characters.');
end;
  is_isogram_lower := to_lower_char(is_isogram_ch);
  if index_of(is_isogram_seen, is_isogram_lower) >= 0 then begin
  exit(false);
end;
  is_isogram_seen := is_isogram_seen + is_isogram_lower;
  is_isogram_i := is_isogram_i + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_isogram('Uncopyrightable'), true)));
  writeln(LowerCase(BoolToStr(is_isogram('allowance'), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
