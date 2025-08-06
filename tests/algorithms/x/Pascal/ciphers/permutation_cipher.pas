{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
  message: string;
  block_size: integer;
  key: IntArray;
  encrypted: string;
  decrypted: string;
  times: integer;
  max: integer;
  message_length: integer;
function rand(max: integer): integer; forward;
function generate_valid_block_size(message_length: integer): integer; forward;
function generate_permutation_key(block_size: integer): IntArray; forward;
function encrypt(message: string; key: IntArray; block_size: integer): string; forward;
function repeat_string(times: integer): StrArray; forward;
function decrypt(encrypted: string; key: IntArray): string; forward;
function rand(max: integer): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483647;
  exit(seed mod max);
end;
function generate_valid_block_size(message_length: integer): integer;
var
  generate_valid_block_size_factors: array of integer;
  generate_valid_block_size_i: integer;
  generate_valid_block_size_idx: integer;
begin
  generate_valid_block_size_factors := [];
  generate_valid_block_size_i := 2;
  while generate_valid_block_size_i <= message_length do begin
  if (message_length mod generate_valid_block_size_i) = 0 then begin
  generate_valid_block_size_factors := concat(generate_valid_block_size_factors, IntArray([generate_valid_block_size_i]));
end;
  generate_valid_block_size_i := generate_valid_block_size_i + 1;
end;
  generate_valid_block_size_idx := rand(Length(generate_valid_block_size_factors));
  exit(generate_valid_block_size_factors[generate_valid_block_size_idx]);
end;
function generate_permutation_key(block_size: integer): IntArray;
var
  generate_permutation_key_digits: array of integer;
  generate_permutation_key_i: integer;
  generate_permutation_key_j: integer;
  generate_permutation_key_k: integer;
  generate_permutation_key_temp: integer;
begin
  generate_permutation_key_digits := [];
  generate_permutation_key_i := 0;
  while generate_permutation_key_i < block_size do begin
  generate_permutation_key_digits := concat(generate_permutation_key_digits, IntArray([generate_permutation_key_i]));
  generate_permutation_key_i := generate_permutation_key_i + 1;
end;
  generate_permutation_key_j := block_size - 1;
  while generate_permutation_key_j > 0 do begin
  generate_permutation_key_k := rand(generate_permutation_key_j + 1);
  generate_permutation_key_temp := generate_permutation_key_digits[generate_permutation_key_j];
  generate_permutation_key_digits[generate_permutation_key_j] := generate_permutation_key_digits[generate_permutation_key_k];
  generate_permutation_key_digits[generate_permutation_key_k] := generate_permutation_key_temp;
  generate_permutation_key_j := generate_permutation_key_j - 1;
end;
  exit(generate_permutation_key_digits);
end;
function encrypt(message: string; key: IntArray; block_size: integer): string;
var
  encrypt_encrypted: string;
  encrypt_i: integer;
  encrypt_block: string;
  encrypt_j: integer;
begin
  encrypt_encrypted := '';
  encrypt_i := 0;
  while encrypt_i < Length(message) do begin
  encrypt_block := copy(message, encrypt_i+1, (encrypt_i + block_size - (encrypt_i)));
  encrypt_j := 0;
  while encrypt_j < block_size do begin
  encrypt_encrypted := encrypt_encrypted + copy(encrypt_block, key[encrypt_j]+1, (key[encrypt_j] + 1 - (key[encrypt_j])));
  encrypt_j := encrypt_j + 1;
end;
  encrypt_i := encrypt_i + block_size;
end;
  exit(encrypt_encrypted);
end;
function repeat_string(times: integer): StrArray;
var
  repeat_string_res: array of string;
  repeat_string_i: integer;
begin
  repeat_string_res := [];
  repeat_string_i := 0;
  while repeat_string_i < times do begin
  repeat_string_res := concat(repeat_string_res, StrArray(['']));
  repeat_string_i := repeat_string_i + 1;
end;
  exit(repeat_string_res);
end;
function decrypt(encrypted: string; key: IntArray): string;
var
  decrypt_klen: integer;
  decrypt_decrypted: string;
  decrypt_i: integer;
  decrypt_block: string;
  decrypt_original: StrArray;
  decrypt_j: integer;
begin
  decrypt_klen := Length(key);
  decrypt_decrypted := '';
  decrypt_i := 0;
  while decrypt_i < Length(encrypted) do begin
  decrypt_block := copy(encrypted, decrypt_i+1, (decrypt_i + decrypt_klen - (decrypt_i)));
  decrypt_original := repeat_string(decrypt_klen);
  decrypt_j := 0;
  while decrypt_j < decrypt_klen do begin
  decrypt_original[key[decrypt_j]] := copy(decrypt_block, decrypt_j+1, (decrypt_j + 1 - (decrypt_j)));
  decrypt_j := decrypt_j + 1;
end;
  decrypt_j := 0;
  while decrypt_j < decrypt_klen do begin
  decrypt_decrypted := decrypt_decrypted + decrypt_original[decrypt_j];
  decrypt_j := decrypt_j + 1;
end;
  decrypt_i := decrypt_i + decrypt_klen;
end;
  exit(decrypt_decrypted);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  message := 'HELLO WORLD';
  block_size := generate_valid_block_size(Length(message));
  key := generate_permutation_key(block_size);
  encrypted := encrypt(message, key, block_size);
  decrypted := decrypt(encrypted, key);
  writeln('Block size: ' + IntToStr(block_size));
  writeln('Key: ' + list_int_to_str(key));
  writeln('Encrypted: ' + encrypted);
  writeln('Decrypted: ' + decrypted);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
