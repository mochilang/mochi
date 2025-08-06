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
  ASCII_UPPERCASE: string;
  ASCII_LOWERCASE: string;
  NEG_ONE: integer;
  s: string;
  ch: string;
  text: string;
  key: string;
function index_of(s: string; ch: string): integer; forward;
function to_uppercase(s: string): string; forward;
function gronsfeld(text: string; key: string): string; forward;
function index_of(s: string; ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if copy(s, index_of_i+1, (index_of_i + 1 - (index_of_i))) = ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(NEG_ONE);
end;
function to_uppercase(s: string): string;
var
  to_uppercase_result_: string;
  to_uppercase_i: integer;
  to_uppercase_ch: string;
  to_uppercase_idx: integer;
begin
  to_uppercase_result_ := '';
  to_uppercase_i := 0;
  while to_uppercase_i < Length(s) do begin
  to_uppercase_ch := copy(s, to_uppercase_i+1, (to_uppercase_i + 1 - (to_uppercase_i)));
  to_uppercase_idx := index_of(ASCII_LOWERCASE, to_uppercase_ch);
  if to_uppercase_idx = NEG_ONE then begin
  to_uppercase_result_ := to_uppercase_result_ + to_uppercase_ch;
end else begin
  to_uppercase_result_ := to_uppercase_result_ + copy(ASCII_UPPERCASE, to_uppercase_idx+1, (to_uppercase_idx + 1 - (to_uppercase_idx)));
end;
  to_uppercase_i := to_uppercase_i + 1;
end;
  exit(to_uppercase_result_);
end;
function gronsfeld(text: string; key: string): string;
var
  gronsfeld_ascii_len: integer;
  gronsfeld_key_len: integer;
  gronsfeld_upper_text: string;
  gronsfeld_encrypted: string;
  gronsfeld_i: integer;
  gronsfeld_ch: string;
  gronsfeld_idx: integer;
  gronsfeld_key_idx: integer;
  gronsfeld_shift: integer;
  gronsfeld_new_position: integer;
begin
  gronsfeld_ascii_len := Length(ASCII_UPPERCASE);
  gronsfeld_key_len := Length(key);
  if gronsfeld_key_len = 0 then begin
  panic('integer modulo by zero');
end;
  gronsfeld_upper_text := to_uppercase(text);
  gronsfeld_encrypted := '';
  gronsfeld_i := 0;
  while gronsfeld_i < Length(gronsfeld_upper_text) do begin
  gronsfeld_ch := copy(gronsfeld_upper_text, gronsfeld_i+1, (gronsfeld_i + 1 - (gronsfeld_i)));
  gronsfeld_idx := index_of(ASCII_UPPERCASE, gronsfeld_ch);
  if gronsfeld_idx = NEG_ONE then begin
  gronsfeld_encrypted := gronsfeld_encrypted + gronsfeld_ch;
end else begin
  gronsfeld_key_idx := gronsfeld_i mod gronsfeld_key_len;
  gronsfeld_shift := StrToInt(copy(key, gronsfeld_key_idx+1, (gronsfeld_key_idx + 1 - (gronsfeld_key_idx))));
  gronsfeld_new_position := (gronsfeld_idx + gronsfeld_shift) mod gronsfeld_ascii_len;
  gronsfeld_encrypted := gronsfeld_encrypted + copy(ASCII_UPPERCASE, gronsfeld_new_position+1, (gronsfeld_new_position + 1 - (gronsfeld_new_position)));
end;
  gronsfeld_i := gronsfeld_i + 1;
end;
  exit(gronsfeld_encrypted);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ASCII_UPPERCASE := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ASCII_LOWERCASE := 'abcdefghijklmnopqrstuvwxyz';
  NEG_ONE := 0 - 1;
  writeln(gronsfeld('hello', '412'));
  writeln(gronsfeld('hello', '123'));
  writeln(gronsfeld('', '123'));
  writeln(gronsfeld('yes, ¥€$ - _!@#%?', '0'));
  writeln(gronsfeld('yes, ¥€$ - _!@#%?', '01'));
  writeln(gronsfeld('yes, ¥€$ - _!@#%?', '012'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
