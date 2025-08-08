{$mode objfpc}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
type BoolArrayArray = array of BoolArray;
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
  b: boolean;
  s: string;
  n: integer;
  ch: string;
  a: string;
  c: string;
function index_of(s: string; ch: string): integer; forward;
function ord_(ch: string): integer; forward;
function chr(n: integer): string; forward;
function to_upper_char(c: string): string; forward;
function is_lower(c: string): boolean; forward;
function abbr(a: string; b: string): boolean; forward;
procedure print_bool(b: boolean); forward;
function index_of(s: string; ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if s[index_of_i+1] = ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function ord_(ch: string): integer;
var
  ord__upper: string;
  ord__lower: string;
  ord__idx: integer;
begin
  ord__upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ord__lower := 'abcdefghijklmnopqrstuvwxyz';
  ord__idx := index_of(ord__upper, ch);
  if ord__idx >= 0 then begin
  exit(65 + ord__idx);
end;
  ord__idx := index_of(ord__lower, ch);
  if ord__idx >= 0 then begin
  exit(97 + ord__idx);
end;
  exit(0);
end;
function chr(n: integer): string;
var
  chr_upper: string;
  chr_lower: string;
begin
  chr_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  chr_lower := 'abcdefghijklmnopqrstuvwxyz';
  if (n >= 65) and (n < 91) then begin
  exit(copy(chr_upper, n - 65+1, (n - 64 - (n - 65))));
end;
  if (n >= 97) and (n < 123) then begin
  exit(copy(chr_lower, n - 97+1, (n - 96 - (n - 97))));
end;
  exit('?');
end;
function to_upper_char(c: string): string;
var
  to_upper_char_code: integer;
begin
  to_upper_char_code := ord_(c);
  if (to_upper_char_code >= 97) and (to_upper_char_code <= 122) then begin
  exit(chr(to_upper_char_code - 32));
end;
  exit(c);
end;
function is_lower(c: string): boolean;
var
  is_lower_code: integer;
begin
  is_lower_code := ord_(c);
  exit((is_lower_code >= 97) and (is_lower_code <= 122));
end;
function abbr(a: string; b: string): boolean;
var
  abbr_n: integer;
  abbr_m: integer;
  abbr_dp: array of BoolArray;
  abbr_i: integer;
  abbr_row: array of boolean;
  abbr_j: integer;
begin
  abbr_n := Length(a);
  abbr_m := Length(b);
  abbr_dp := [];
  abbr_i := 0;
  while abbr_i <= abbr_n do begin
  abbr_row := [];
  abbr_j := 0;
  while abbr_j <= abbr_m do begin
  abbr_row := concat(abbr_row, [false]);
  abbr_j := abbr_j + 1;
end;
  abbr_dp := concat(abbr_dp, [abbr_row]);
  abbr_i := abbr_i + 1;
end;
  abbr_dp[0][0] := true;
  abbr_i := 0;
  while abbr_i < abbr_n do begin
  abbr_j := 0;
  while abbr_j <= abbr_m do begin
  if abbr_dp[abbr_i][abbr_j] then begin
  if (abbr_j < abbr_m) and (to_upper_char(a[abbr_i+1]) = b[abbr_j+1]) then begin
  abbr_dp[abbr_i + 1][abbr_j + 1] := true;
end;
  if is_lower(a[abbr_i+1]) then begin
  abbr_dp[abbr_i + 1][abbr_j] := true;
end;
end;
  abbr_j := abbr_j + 1;
end;
  abbr_i := abbr_i + 1;
end;
  exit(abbr_dp[abbr_n][abbr_m]);
end;
procedure print_bool(b: boolean);
begin
  if b then begin
  writeln(Ord(true));
end else begin
  writeln(Ord(false));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  print_bool(abbr('daBcd', 'ABC'));
  print_bool(abbr('dBcd', 'ABC'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
