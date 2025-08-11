{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
  logins1: array of string;
  logins2: array of string;
  chars: StrArray;
  permutation: StrArray;
  idx: integer;
  xs: StrArray;
  c: string;
  s: string;
  current: StrArray;
  logins: StrArray;
function parse_int(s: string): integer; forward;
function join(xs: StrArray): string; forward;
function contains(xs: StrArray; c: string): boolean; forward;
function index_of(xs: StrArray; c: string): integer; forward;
function remove_at(xs: StrArray; idx: integer): StrArray; forward;
function unique_chars(logins: StrArray): StrArray; forward;
function satisfies(permutation: StrArray; logins: StrArray): boolean; forward;
function search(chars: StrArray; current: StrArray; logins: StrArray): string; forward;
function find_secret_passcode(logins: StrArray): integer; forward;
function parse_int(s: string): integer;
var
  parse_int_value: integer;
  parse_int_i: integer;
  parse_int_c: string;
begin
  parse_int_value := 0;
  parse_int_i := 0;
  while parse_int_i < Length(s) do begin
  parse_int_c := s[parse_int_i+1];
  parse_int_value := (parse_int_value * 10) + StrToInt(parse_int_c);
  parse_int_i := parse_int_i + 1;
end;
  exit(parse_int_value);
end;
function join(xs: StrArray): string;
var
  join_s: string;
  join_i: integer;
begin
  join_s := '';
  join_i := 0;
  while join_i < Length(xs) do begin
  join_s := join_s + xs[join_i];
  join_i := join_i + 1;
end;
  exit(join_s);
end;
function contains(xs: StrArray; c: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = c then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function index_of(xs: StrArray; c: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(xs) do begin
  if xs[index_of_i] = c then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function remove_at(xs: StrArray; idx: integer): StrArray;
var
  remove_at_res: array of string;
  remove_at_i: integer;
begin
  remove_at_res := [];
  remove_at_i := 0;
  while remove_at_i < Length(xs) do begin
  if remove_at_i <> idx then begin
  remove_at_res := concat(remove_at_res, StrArray([xs[remove_at_i]]));
end;
  remove_at_i := remove_at_i + 1;
end;
  exit(remove_at_res);
end;
function unique_chars(logins: StrArray): StrArray;
var
  unique_chars_chars: array of string;
  unique_chars_i: integer;
  unique_chars_login: string;
  unique_chars_j: integer;
  unique_chars_c: string;
begin
  unique_chars_chars := [];
  unique_chars_i := 0;
  while unique_chars_i < Length(logins) do begin
  unique_chars_login := logins[unique_chars_i];
  unique_chars_j := 0;
  while unique_chars_j < Length(unique_chars_login) do begin
  unique_chars_c := unique_chars_login[unique_chars_j+1];
  if not contains(unique_chars_chars, unique_chars_c) then begin
  unique_chars_chars := concat(unique_chars_chars, StrArray([unique_chars_c]));
end;
  unique_chars_j := unique_chars_j + 1;
end;
  unique_chars_i := unique_chars_i + 1;
end;
  exit(unique_chars_chars);
end;
function satisfies(permutation: StrArray; logins: StrArray): boolean;
var
  satisfies_i: integer;
  satisfies_login: string;
  satisfies_i0: integer;
  satisfies_i1: integer;
  satisfies_i2: integer;
begin
  satisfies_i := 0;
  while satisfies_i < Length(logins) do begin
  satisfies_login := logins[satisfies_i];
  satisfies_i0 := index_of(permutation, satisfies_login[0+1]);
  satisfies_i1 := index_of(permutation, satisfies_login[1+1]);
  satisfies_i2 := index_of(permutation, satisfies_login[2+1]);
  if not ((satisfies_i0 < satisfies_i1) and (satisfies_i1 < satisfies_i2)) then begin
  exit(false);
end;
  satisfies_i := satisfies_i + 1;
end;
  exit(true);
end;
function search(chars: StrArray; current: StrArray; logins: StrArray): string;
var
  search_i: integer;
  search_c: string;
  search_rest: StrArray;
  search_next: array of string;
  search_res: string;
begin
  if Length(chars) = 0 then begin
  if satisfies(current, logins) then begin
  exit(join(current));
end;
  exit('');
end;
  search_i := 0;
  while search_i < Length(chars) do begin
  search_c := chars[search_i];
  search_rest := remove_at(chars, search_i);
  search_next := concat(current, StrArray([search_c]));
  search_res := search(search_rest, search_next, logins);
  if search_res <> '' then begin
  exit(search_res);
end;
  search_i := search_i + 1;
end;
  exit('');
end;
function find_secret_passcode(logins: StrArray): integer;
var
  find_secret_passcode_chars: StrArray;
  find_secret_passcode_s: string;
begin
  find_secret_passcode_chars := unique_chars(logins);
  find_secret_passcode_s := search(find_secret_passcode_chars, [], logins);
  if find_secret_passcode_s = '' then begin
  exit(-1);
end;
  exit(parse_int(find_secret_passcode_s));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  logins1 := ['135', '259', '235', '189', '690', '168', '120', '136', '289', '589', '160', '165', '580', '369', '250', '280'];
  writeln(IntToStr(find_secret_passcode(logins1)));
  logins2 := ['426', '281', '061', '819', '268', '406', '420', '428', '209', '689', '019', '421', '469', '261', '681', '201'];
  writeln(IntToStr(find_secret_passcode(logins2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
