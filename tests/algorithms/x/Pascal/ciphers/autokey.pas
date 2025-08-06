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
  LOWER: string;
  UPPER: string;
  c: string;
  i: integer;
  s: string;
  key: string;
  plaintext: string;
  ciphertext: string;
function to_lowercase(s: string): string; forward;
function char_index(c: string): integer; forward;
function index_char(i: integer): string; forward;
function encrypt(plaintext: string; key: string): string; forward;
function decrypt(ciphertext: string; key: string): string; forward;
function to_lowercase(s: string): string;
var
  to_lowercase_res: string;
  to_lowercase_i: integer;
  to_lowercase_c: string;
  to_lowercase_j: integer;
  to_lowercase_found: boolean;
begin
  to_lowercase_res := '';
  to_lowercase_i := 0;
  while to_lowercase_i < Length(s) do begin
  to_lowercase_c := s[to_lowercase_i+1];
  to_lowercase_j := 0;
  to_lowercase_found := false;
  while to_lowercase_j < 26 do begin
  if to_lowercase_c = UPPER[to_lowercase_j+1] then begin
  to_lowercase_res := to_lowercase_res + LOWER[to_lowercase_j+1];
  to_lowercase_found := true;
  break;
end;
  to_lowercase_j := to_lowercase_j + 1;
end;
  if not to_lowercase_found then begin
  to_lowercase_res := to_lowercase_res + to_lowercase_c;
end;
  to_lowercase_i := to_lowercase_i + 1;
end;
  exit(to_lowercase_res);
end;
function char_index(c: string): integer;
var
  char_index_i: integer;
begin
  char_index_i := 0;
  while char_index_i < 26 do begin
  if c = LOWER[char_index_i+1] then begin
  exit(char_index_i);
end;
  char_index_i := char_index_i + 1;
end;
  exit(-1);
end;
function index_char(i: integer): string;
begin
  exit(LOWER[i+1]);
end;
function encrypt(plaintext: string; key: string): string;
var
  encrypt_full_key: string;
  encrypt_p_i: integer;
  encrypt_k_i: integer;
  encrypt_ciphertext: string;
  encrypt_p_char: string;
  encrypt_p_idx: integer;
  encrypt_k_char: string;
  encrypt_k_idx: integer;
  encrypt_c_idx: integer;
begin
  if Length(plaintext) = 0 then begin
  panic('plaintext is empty');
end;
  if Length(key) = 0 then begin
  panic('key is empty');
end;
  encrypt_full_key := key + plaintext;
  plaintext := to_lowercase(plaintext);
  encrypt_full_key := to_lowercase(encrypt_full_key);
  encrypt_p_i := 0;
  encrypt_k_i := 0;
  encrypt_ciphertext := '';
  while encrypt_p_i < Length(plaintext) do begin
  encrypt_p_char := plaintext[encrypt_p_i+1];
  encrypt_p_idx := char_index(encrypt_p_char);
  if encrypt_p_idx < 0 then begin
  encrypt_ciphertext := encrypt_ciphertext + encrypt_p_char;
  encrypt_p_i := encrypt_p_i + 1;
end else begin
  encrypt_k_char := encrypt_full_key[encrypt_k_i+1];
  encrypt_k_idx := char_index(encrypt_k_char);
  if encrypt_k_idx < 0 then begin
  encrypt_k_i := encrypt_k_i + 1;
end else begin
  encrypt_c_idx := (encrypt_p_idx + encrypt_k_idx) mod 26;
  encrypt_ciphertext := encrypt_ciphertext + index_char(encrypt_c_idx);
  encrypt_k_i := encrypt_k_i + 1;
  encrypt_p_i := encrypt_p_i + 1;
end;
end;
end;
  exit(encrypt_ciphertext);
end;
function decrypt(ciphertext: string; key: string): string;
var
  decrypt_current_key: string;
  decrypt_c_i: integer;
  decrypt_k_i: integer;
  decrypt_plaintext: string;
  decrypt_c_char: string;
  decrypt_c_idx: integer;
  decrypt_k_char: string;
  decrypt_k_idx: integer;
  decrypt_p_idx: integer;
  decrypt_p_char: string;
begin
  if Length(ciphertext) = 0 then begin
  panic('ciphertext is empty');
end;
  if Length(key) = 0 then begin
  panic('key is empty');
end;
  decrypt_current_key := to_lowercase(key);
  decrypt_c_i := 0;
  decrypt_k_i := 0;
  decrypt_plaintext := '';
  while decrypt_c_i < Length(ciphertext) do begin
  decrypt_c_char := ciphertext[decrypt_c_i+1];
  decrypt_c_idx := char_index(decrypt_c_char);
  if decrypt_c_idx < 0 then begin
  decrypt_plaintext := decrypt_plaintext + decrypt_c_char;
end else begin
  decrypt_k_char := decrypt_current_key[decrypt_k_i+1];
  decrypt_k_idx := char_index(decrypt_k_char);
  decrypt_p_idx := ((decrypt_c_idx - decrypt_k_idx) + 26) mod 26;
  decrypt_p_char := index_char(decrypt_p_idx);
  decrypt_plaintext := decrypt_plaintext + decrypt_p_char;
  decrypt_current_key := decrypt_current_key + decrypt_p_char;
  decrypt_k_i := decrypt_k_i + 1;
end;
  decrypt_c_i := decrypt_c_i + 1;
end;
  exit(decrypt_plaintext);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  writeln(encrypt('hello world', 'coffee'));
  writeln(decrypt('jsqqs avvwo', 'coffee'));
  writeln(encrypt('coffee is good as python', 'TheAlgorithms'));
  writeln(decrypt('vvjfpk wj ohvp su ddylsv', 'TheAlgorithms'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
