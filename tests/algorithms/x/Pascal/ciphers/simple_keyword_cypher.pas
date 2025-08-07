{$mode objfpc}
program Main;
type StrArray = array of string;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
var
  cipher_map: StrArray;
  encoded: string;
function index_in_string(index_in_string_s: string; index_in_string_ch: string): integer; forward;
function contains_char(contains_char_s: string; contains_char_ch: string): boolean; forward;
function is_alpha(is_alpha_ch: string): boolean; forward;
function to_upper(to_upper_s: string): string; forward;
function remove_duplicates(remove_duplicates_key: string): string; forward;
function create_cipher_map(create_cipher_map_key: string): StrArray; forward;
function index_in_list(index_in_list_lst: StrArray; index_in_list_ch: string): integer; forward;
function encipher(encipher_message: string; encipher_cipher: StrArray): string; forward;
function decipher(decipher_message: string; decipher_cipher: StrArray): string; forward;
function index_in_string(index_in_string_s: string; index_in_string_ch: string): integer;
var
  index_in_string_i: integer;
begin
  index_in_string_i := 0;
  while index_in_string_i < Length(index_in_string_s) do begin
  if index_in_string_s[index_in_string_i+1] = index_in_string_ch then begin
  exit(index_in_string_i);
end;
  index_in_string_i := index_in_string_i + 1;
end;
  exit(-1);
end;
function contains_char(contains_char_s: string; contains_char_ch: string): boolean;
begin
  exit(index_in_string(contains_char_s, contains_char_ch) >= 0);
end;
function is_alpha(is_alpha_ch: string): boolean;
var
  is_alpha_lower: string;
  is_alpha_upper: string;
begin
  is_alpha_lower := 'abcdefghijklmnopqrstuvwxyz';
  is_alpha_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  exit(contains_char(is_alpha_lower, is_alpha_ch) or contains_char(is_alpha_upper, is_alpha_ch));
end;
function to_upper(to_upper_s: string): string;
var
  to_upper_lower: string;
  to_upper_upper: string;
  to_upper_res: string;
  to_upper_i: integer;
  to_upper_ch: string;
  to_upper_idx: integer;
begin
  to_upper_lower := 'abcdefghijklmnopqrstuvwxyz';
  to_upper_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  to_upper_res := '';
  to_upper_i := 0;
  while to_upper_i < Length(to_upper_s) do begin
  to_upper_ch := to_upper_s[to_upper_i+1];
  to_upper_idx := index_in_string(to_upper_lower, to_upper_ch);
  if to_upper_idx >= 0 then begin
  to_upper_res := to_upper_res + to_upper_upper[to_upper_idx+1];
end else begin
  to_upper_res := to_upper_res + to_upper_ch;
end;
  to_upper_i := to_upper_i + 1;
end;
  exit(to_upper_res);
end;
function remove_duplicates(remove_duplicates_key: string): string;
var
  remove_duplicates_res: string;
  remove_duplicates_i: integer;
  remove_duplicates_ch: string;
begin
  remove_duplicates_res := '';
  remove_duplicates_i := 0;
  while remove_duplicates_i < Length(remove_duplicates_key) do begin
  remove_duplicates_ch := remove_duplicates_key[remove_duplicates_i+1];
  if (remove_duplicates_ch = ' ') or (is_alpha(remove_duplicates_ch) and (contains_char(remove_duplicates_res, remove_duplicates_ch) = false)) then begin
  remove_duplicates_res := remove_duplicates_res + remove_duplicates_ch;
end;
  remove_duplicates_i := remove_duplicates_i + 1;
end;
  exit(remove_duplicates_res);
end;
function create_cipher_map(create_cipher_map_key: string): StrArray;
var
  create_cipher_map_alphabet: string;
  create_cipher_map_cleaned: string;
  create_cipher_map_cipher: array of string;
  create_cipher_map_i: integer;
  create_cipher_map_offset: integer;
  create_cipher_map_j: integer;
  create_cipher_map_char: string;
begin
  create_cipher_map_alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  create_cipher_map_cleaned := remove_duplicates(to_upper(create_cipher_map_key));
  create_cipher_map_cipher := [];
  create_cipher_map_i := 0;
  while create_cipher_map_i < Length(create_cipher_map_cleaned) do begin
  create_cipher_map_cipher := concat(create_cipher_map_cipher, StrArray([create_cipher_map_cleaned[create_cipher_map_i+1]]));
  create_cipher_map_i := create_cipher_map_i + 1;
end;
  create_cipher_map_offset := Length(create_cipher_map_cleaned);
  create_cipher_map_j := Length(create_cipher_map_cipher);
  while create_cipher_map_j < 26 do begin
  create_cipher_map_char := create_cipher_map_alphabet[create_cipher_map_j - create_cipher_map_offset+1];
  while contains_char(create_cipher_map_cleaned, create_cipher_map_char) do begin
  create_cipher_map_offset := create_cipher_map_offset - 1;
  create_cipher_map_char := create_cipher_map_alphabet[create_cipher_map_j - create_cipher_map_offset+1];
end;
  create_cipher_map_cipher := concat(create_cipher_map_cipher, StrArray([create_cipher_map_char]));
  create_cipher_map_j := create_cipher_map_j + 1;
end;
  exit(create_cipher_map_cipher);
end;
function index_in_list(index_in_list_lst: StrArray; index_in_list_ch: string): integer;
var
  index_in_list_i: integer;
begin
  index_in_list_i := 0;
  while index_in_list_i < Length(index_in_list_lst) do begin
  if index_in_list_lst[index_in_list_i] = index_in_list_ch then begin
  exit(index_in_list_i);
end;
  index_in_list_i := index_in_list_i + 1;
end;
  exit(-1);
end;
function encipher(encipher_message: string; encipher_cipher: StrArray): string;
var
  encipher_alphabet: string;
  encipher_msg: string;
  encipher_res: string;
  encipher_i: integer;
  encipher_ch: string;
  encipher_idx: integer;
begin
  encipher_alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  encipher_msg := to_upper(encipher_message);
  encipher_res := '';
  encipher_i := 0;
  while encipher_i < Length(encipher_msg) do begin
  encipher_ch := encipher_msg[encipher_i+1];
  encipher_idx := index_in_string(encipher_alphabet, encipher_ch);
  if encipher_idx >= 0 then begin
  encipher_res := encipher_res + encipher_cipher[encipher_idx];
end else begin
  encipher_res := encipher_res + encipher_ch;
end;
  encipher_i := encipher_i + 1;
end;
  exit(encipher_res);
end;
function decipher(decipher_message: string; decipher_cipher: StrArray): string;
var
  decipher_alphabet: string;
  decipher_msg: string;
  decipher_res: string;
  decipher_i: integer;
  decipher_ch: string;
  decipher_idx: integer;
begin
  decipher_alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  decipher_msg := to_upper(decipher_message);
  decipher_res := '';
  decipher_i := 0;
  while decipher_i < Length(decipher_msg) do begin
  decipher_ch := decipher_msg[decipher_i+1];
  decipher_idx := index_in_list(decipher_cipher, decipher_ch);
  if decipher_idx >= 0 then begin
  decipher_res := decipher_res + decipher_alphabet[decipher_idx+1];
end else begin
  decipher_res := decipher_res + decipher_ch;
end;
  decipher_i := decipher_i + 1;
end;
  exit(decipher_res);
end;
begin
  cipher_map := create_cipher_map('Goodbye!!');
  encoded := encipher('Hello World!!', cipher_map);
  writeln(encoded);
  writeln(decipher(encoded, cipher_map));
end.
