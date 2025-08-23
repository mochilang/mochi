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
  MAX_LOCAL_PART_OCTETS: integer;
  MAX_DOMAIN_OCTETS: integer;
  ASCII_LETTERS: string;
  DIGITS: string;
  LOCAL_EXTRA: string;
  DOMAIN_EXTRA: string;
  email_tests: array of string;
  idx: integer;
  email: string;
function count_char(count_char_s: string; count_char_target: string): integer; forward;
function char_in(char_in_c: string; char_in_allowed: string): boolean; forward;
function starts_with_char(starts_with_char_s: string; starts_with_char_c: string): boolean; forward;
function ends_with_char(ends_with_char_s: string; ends_with_char_c: string): boolean; forward;
function contains_double_dot(contains_double_dot_s: string): boolean; forward;
function is_valid_email_address(is_valid_email_address_email: string): boolean; forward;
function count_char(count_char_s: string; count_char_target: string): integer;
var
  count_char_cnt: integer;
  count_char_i: integer;
begin
  count_char_cnt := 0;
  count_char_i := 0;
  while count_char_i < Length(count_char_s) do begin
  if copy(count_char_s, count_char_i+1, (count_char_i + 1 - (count_char_i))) = count_char_target then begin
  count_char_cnt := count_char_cnt + 1;
end;
  count_char_i := count_char_i + 1;
end;
  exit(count_char_cnt);
end;
function char_in(char_in_c: string; char_in_allowed: string): boolean;
var
  char_in_i: integer;
begin
  char_in_i := 0;
  while char_in_i < Length(char_in_allowed) do begin
  if copy(char_in_allowed, char_in_i+1, (char_in_i + 1 - (char_in_i))) = char_in_c then begin
  exit(true);
end;
  char_in_i := char_in_i + 1;
end;
  exit(false);
end;
function starts_with_char(starts_with_char_s: string; starts_with_char_c: string): boolean;
begin
  exit((Length(starts_with_char_s) > 0) and (copy(starts_with_char_s, 1, 1) = starts_with_char_c));
end;
function ends_with_char(ends_with_char_s: string; ends_with_char_c: string): boolean;
begin
  exit((Length(ends_with_char_s) > 0) and (copy(ends_with_char_s, Length(ends_with_char_s) - 1+1, (Length(ends_with_char_s) - (Length(ends_with_char_s) - 1))) = ends_with_char_c));
end;
function contains_double_dot(contains_double_dot_s: string): boolean;
var
  contains_double_dot_i: integer;
begin
  if Length(contains_double_dot_s) < 2 then begin
  exit(false);
end;
  contains_double_dot_i := 0;
  while contains_double_dot_i < (Length(contains_double_dot_s) - 1) do begin
  if copy(contains_double_dot_s, contains_double_dot_i+1, (contains_double_dot_i + 2 - (contains_double_dot_i))) = '..' then begin
  exit(true);
end;
  contains_double_dot_i := contains_double_dot_i + 1;
end;
  exit(false);
end;
function is_valid_email_address(is_valid_email_address_email: string): boolean;
var
  is_valid_email_address_at_idx: integer;
  is_valid_email_address_i: integer;
  is_valid_email_address_local_part: string;
  is_valid_email_address_domain: string;
  is_valid_email_address_ch: string;
begin
  if count_char(is_valid_email_address_email, '@') <> 1 then begin
  exit(false);
end;
  is_valid_email_address_at_idx := 0;
  is_valid_email_address_i := 0;
  while is_valid_email_address_i < Length(is_valid_email_address_email) do begin
  if copy(is_valid_email_address_email, is_valid_email_address_i+1, (is_valid_email_address_i + 1 - (is_valid_email_address_i))) = '@' then begin
  is_valid_email_address_at_idx := is_valid_email_address_i;
  break;
end;
  is_valid_email_address_i := is_valid_email_address_i + 1;
end;
  is_valid_email_address_local_part := copy(is_valid_email_address_email, 1, (is_valid_email_address_at_idx - (0)));
  is_valid_email_address_domain := copy(is_valid_email_address_email, is_valid_email_address_at_idx + 1+1, (Length(is_valid_email_address_email) - (is_valid_email_address_at_idx + 1)));
  if (Length(is_valid_email_address_local_part) > MAX_LOCAL_PART_OCTETS) or (Length(is_valid_email_address_domain) > MAX_DOMAIN_OCTETS) then begin
  exit(false);
end;
  is_valid_email_address_i := 0;
  while is_valid_email_address_i < Length(is_valid_email_address_local_part) do begin
  is_valid_email_address_ch := copy(is_valid_email_address_local_part, is_valid_email_address_i+1, (is_valid_email_address_i + 1 - (is_valid_email_address_i)));
  if not char_in(is_valid_email_address_ch, (ASCII_LETTERS + DIGITS) + LOCAL_EXTRA) then begin
  exit(false);
end;
  is_valid_email_address_i := is_valid_email_address_i + 1;
end;
  if (starts_with_char(is_valid_email_address_local_part, '.') or ends_with_char(is_valid_email_address_local_part, '.')) or contains_double_dot(is_valid_email_address_local_part) then begin
  exit(false);
end;
  is_valid_email_address_i := 0;
  while is_valid_email_address_i < Length(is_valid_email_address_domain) do begin
  is_valid_email_address_ch := copy(is_valid_email_address_domain, is_valid_email_address_i+1, (is_valid_email_address_i + 1 - (is_valid_email_address_i)));
  if not char_in(is_valid_email_address_ch, (ASCII_LETTERS + DIGITS) + DOMAIN_EXTRA) then begin
  exit(false);
end;
  is_valid_email_address_i := is_valid_email_address_i + 1;
end;
  if starts_with_char(is_valid_email_address_domain, '-') or ends_with_char(is_valid_email_address_domain, '.') then begin
  exit(false);
end;
  if (starts_with_char(is_valid_email_address_domain, '.') or ends_with_char(is_valid_email_address_domain, '.')) or contains_double_dot(is_valid_email_address_domain) then begin
  exit(false);
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  MAX_LOCAL_PART_OCTETS := 64;
  MAX_DOMAIN_OCTETS := 255;
  ASCII_LETTERS := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  DIGITS := '0123456789';
  LOCAL_EXTRA := '.(!#$%&''*+-/=?^_`{|}~)';
  DOMAIN_EXTRA := '.-';
  email_tests := ['simple@example.com', 'very.common@example.com', 'disposable.style.email.with+symbol@example.com', 'other-email-with-hyphen@and.subdomains.example.com', 'fully-qualified-domain@example.com', 'user.name+tag+sorting@example.com', 'x@example.com', 'example-indeed@strange-example.com', 'test/test@test.com', '123456789012345678901234567890123456789012345678901234567890123@example.com', 'admin@mailserver1', 'example@s.example', 'Abc.example.com', 'A@b@c@example.com', 'abc@example..com', 'a(c)d,e:f;g<h>i[j\k]l@example.com', '12345678901234567890123456789012345678901234567890123456789012345@example.com', 'i.like.underscores@but_its_not_allowed_in_this_part', ''];
  idx := 0;
  while idx < Length(email_tests) do begin
  email := email_tests[idx];
  writeln(LowerCase(BoolToStr(is_valid_email_address(email), true)));
  idx := idx + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
