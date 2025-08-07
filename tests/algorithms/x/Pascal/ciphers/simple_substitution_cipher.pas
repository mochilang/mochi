{$mode objfpc}
program Main;
uses fgl;
type StrArray = array of string;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
var
  LETTERS: string;
  LOWERCASE: string;
  seed: integer;
  key: string;
function rand(rand_n: integer): integer; forward;
function get_random_key(): string; forward;
function check_valid_key(check_valid_key_key: string): boolean; forward;
function index_in(index_in_s: string; index_in_ch: string): integer; forward;
function char_to_upper(char_to_upper_c: string): string; forward;
function char_to_lower(char_to_lower_c: string): string; forward;
function is_upper(is_upper_c: string): boolean; forward;
function translate_message(translate_message_key: string; translate_message_message: string; translate_message_mode: string): string; forward;
function encrypt_message(encrypt_message_key: string; encrypt_message_message: string): string; forward;
function decrypt_message(decrypt_message_key: string; decrypt_message_message: string): string; forward;
function rand(rand_n: integer): integer;
begin
  seed := ((seed * 1664525) + 1013904223) mod 2147483647;
  exit(seed mod rand_n);
end;
function get_random_key(): string;
var
  get_random_key_chars: array of string;
  get_random_key_i: integer;
  get_random_key_j: integer;
  get_random_key_k: integer;
  get_random_key_tmp: string;
  get_random_key_res: string;
begin
  get_random_key_i := 0;
  while get_random_key_i < Length(LETTERS) do begin
  get_random_key_chars := concat(get_random_key_chars, StrArray([LETTERS[get_random_key_i+1]]));
  get_random_key_i := get_random_key_i + 1;
end;
  get_random_key_j := Length(get_random_key_chars) - 1;
  while get_random_key_j > 0 do begin
  get_random_key_k := rand(get_random_key_j + 1);
  get_random_key_tmp := get_random_key_chars[get_random_key_j];
  get_random_key_chars[get_random_key_j] := get_random_key_chars[get_random_key_k];
  get_random_key_chars[get_random_key_k] := get_random_key_tmp;
  get_random_key_j := get_random_key_j - 1;
end;
  get_random_key_res := '';
  get_random_key_i := 0;
  while get_random_key_i < Length(get_random_key_chars) do begin
  get_random_key_res := get_random_key_res + get_random_key_chars[get_random_key_i];
  get_random_key_i := get_random_key_i + 1;
end;
  exit(get_random_key_res);
end;
function check_valid_key(check_valid_key_key: string): boolean;
var
  check_valid_key_used: specialize TFPGMap<string, boolean>;
  check_valid_key_i: integer;
  check_valid_key_ch: string;
begin
  if Length(check_valid_key_key) <> Length(LETTERS) then begin
  exit(false);
end;
  check_valid_key_used := specialize TFPGMap<string, boolean>.Create();
  check_valid_key_i := 0;
  while check_valid_key_i < Length(check_valid_key_key) do begin
  check_valid_key_ch := check_valid_key_key[check_valid_key_i+1];
  if check_valid_key_used[check_valid_key_ch] then begin
  exit(false);
end;
  check_valid_key_used[check_valid_key_ch] := true;
  check_valid_key_i := check_valid_key_i + 1;
end;
  check_valid_key_i := 0;
  while check_valid_key_i < Length(LETTERS) do begin
  check_valid_key_ch := LETTERS[check_valid_key_i+1];
  if not check_valid_key_used[check_valid_key_ch] then begin
  exit(false);
end;
  check_valid_key_i := check_valid_key_i + 1;
end;
  exit(true);
end;
function index_in(index_in_s: string; index_in_ch: string): integer;
var
  index_in_i: integer;
begin
  index_in_i := 0;
  while index_in_i < Length(index_in_s) do begin
  if index_in_s[index_in_i+1] = index_in_ch then begin
  exit(index_in_i);
end;
  index_in_i := index_in_i + 1;
end;
  exit(-1);
end;
function char_to_upper(char_to_upper_c: string): string;
var
  char_to_upper_i: integer;
begin
  char_to_upper_i := 0;
  while char_to_upper_i < Length(LOWERCASE) do begin
  if char_to_upper_c = LOWERCASE[char_to_upper_i+1] then begin
  exit(LETTERS[char_to_upper_i+1]);
end;
  char_to_upper_i := char_to_upper_i + 1;
end;
  exit(char_to_upper_c);
end;
function char_to_lower(char_to_lower_c: string): string;
var
  char_to_lower_i: integer;
begin
  char_to_lower_i := 0;
  while char_to_lower_i < Length(LETTERS) do begin
  if char_to_lower_c = LETTERS[char_to_lower_i+1] then begin
  exit(LOWERCASE[char_to_lower_i+1]);
end;
  char_to_lower_i := char_to_lower_i + 1;
end;
  exit(char_to_lower_c);
end;
function is_upper(is_upper_c: string): boolean;
var
  is_upper_i: integer;
begin
  is_upper_i := 0;
  while is_upper_i < Length(LETTERS) do begin
  if is_upper_c = LETTERS[is_upper_i+1] then begin
  exit(true);
end;
  is_upper_i := is_upper_i + 1;
end;
  exit(false);
end;
function translate_message(translate_message_key: string; translate_message_message: string; translate_message_mode: string): string;
var
  translate_message_chars_a: string;
  translate_message_chars_b: string;
  translate_message_tmp: string;
  translate_message_translated: string;
  translate_message_i: integer;
  translate_message_symbol: string;
  translate_message_upper_symbol: string;
  translate_message_idx: integer;
  translate_message_mapped: string;
begin
  translate_message_chars_a := LETTERS;
  translate_message_chars_b := translate_message_key;
  if translate_message_mode = 'decrypt' then begin
  translate_message_tmp := translate_message_chars_a;
  translate_message_chars_a := translate_message_chars_b;
  translate_message_chars_b := translate_message_tmp;
end;
  translate_message_translated := '';
  translate_message_i := 0;
  while translate_message_i < Length(translate_message_message) do begin
  translate_message_symbol := translate_message_message[translate_message_i+1];
  translate_message_upper_symbol := char_to_upper(translate_message_symbol);
  translate_message_idx := index_in(translate_message_chars_a, translate_message_upper_symbol);
  if translate_message_idx >= 0 then begin
  translate_message_mapped := translate_message_chars_b[translate_message_idx+1];
  if is_upper(translate_message_symbol) then begin
  translate_message_translated := translate_message_translated + translate_message_mapped;
end else begin
  translate_message_translated := translate_message_translated + char_to_lower(translate_message_mapped);
end;
end else begin
  translate_message_translated := translate_message_translated + translate_message_symbol;
end;
  translate_message_i := translate_message_i + 1;
end;
  exit(translate_message_translated);
end;
function encrypt_message(encrypt_message_key: string; encrypt_message_message: string): string;
var
  encrypt_message_res: string;
begin
  encrypt_message_res := translate_message(encrypt_message_key, encrypt_message_message, 'encrypt');
  exit(encrypt_message_res);
end;
function decrypt_message(decrypt_message_key: string; decrypt_message_message: string): string;
var
  decrypt_message_res: string;
begin
  decrypt_message_res := translate_message(decrypt_message_key, decrypt_message_message, 'decrypt');
  exit(decrypt_message_res);
end;
begin
  LETTERS := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LOWERCASE := 'abcdefghijklmnopqrstuvwxyz';
  seed := 1;
  key := 'LFWOAYUISVKMNXPBDCRJTQEGHZ';
  writeln(encrypt_message(key, 'Harshil Darji'));
  writeln(decrypt_message(key, 'Ilcrism Olcvs'));
end.
