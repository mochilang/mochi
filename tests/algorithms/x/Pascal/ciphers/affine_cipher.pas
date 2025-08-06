{$mode objfpc}
program Main;
uses SysUtils, Math;
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
  SYMBOLS: string;
  a: integer;
  key_a: integer;
  key: integer;
  message: string;
  ch: string;
  key_b: integer;
  b: integer;
  mode: string;
  m: integer;
function gcd(a: integer; b: integer): integer; forward;
function mod_inverse(a: integer; m: integer): integer; forward;
function find_symbol(ch: string): integer; forward;
procedure check_keys(key_a: integer; key_b: integer; mode: string); forward;
function encrypt_message(key: integer; message: string): string; forward;
function decrypt_message(key: integer; message: string): string; forward;
procedure main(); forward;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_temp: integer;
begin
  gcd_x := a;
  gcd_y := b;
  while gcd_y <> 0 do begin
  gcd_temp := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_temp;
end;
  exit(gcd_x);
end;
function mod_inverse(a: integer; m: integer): integer;
var
  mod_inverse_u1: integer;
  mod_inverse_u2: integer;
  mod_inverse_u3: integer;
  mod_inverse_v1: integer;
  mod_inverse_v2: integer;
  mod_inverse_v3: integer;
  mod_inverse_q: integer;
  mod_inverse_t1: integer;
  mod_inverse_t2: integer;
  mod_inverse_t3: integer;
  mod_inverse_res: integer;
begin
  if gcd(a, m) <> 1 then begin
  panic(((('mod inverse of ' + IntToStr(a)) + ' and ') + IntToStr(m)) + ' does not exist');
end;
  mod_inverse_u1 := 1;
  mod_inverse_u2 := 0;
  mod_inverse_u3 := a;
  mod_inverse_v1 := 0;
  mod_inverse_v2 := 1;
  mod_inverse_v3 := m;
  while mod_inverse_v3 <> 0 do begin
  mod_inverse_q := mod_inverse_u3 div mod_inverse_v3;
  mod_inverse_t1 := mod_inverse_u1 - (mod_inverse_q * mod_inverse_v1);
  mod_inverse_t2 := mod_inverse_u2 - (mod_inverse_q * mod_inverse_v2);
  mod_inverse_t3 := mod_inverse_u3 - (mod_inverse_q * mod_inverse_v3);
  mod_inverse_u1 := mod_inverse_v1;
  mod_inverse_u2 := mod_inverse_v2;
  mod_inverse_u3 := mod_inverse_v3;
  mod_inverse_v1 := mod_inverse_t1;
  mod_inverse_v2 := mod_inverse_t2;
  mod_inverse_v3 := mod_inverse_t3;
end;
  mod_inverse_res := mod_inverse_u1 mod m;
  if mod_inverse_res < 0 then begin
  exit(mod_inverse_res + m);
end;
  exit(mod_inverse_res);
end;
function find_symbol(ch: string): integer;
var
  find_symbol_i: integer;
begin
  find_symbol_i := 0;
  while find_symbol_i < Length(SYMBOLS) do begin
  if SYMBOLS[find_symbol_i+1] = ch then begin
  exit(find_symbol_i);
end;
  find_symbol_i := find_symbol_i + 1;
end;
  exit(-1);
end;
procedure check_keys(key_a: integer; key_b: integer; mode: string);
var
  check_keys_m: integer;
begin
  check_keys_m := Length(SYMBOLS);
  if mode = 'encrypt' then begin
  if key_a = 1 then begin
  panic('The affine cipher becomes weak when key A is set to 1. Choose different key');
end;
  if key_b = 0 then begin
  panic('The affine cipher becomes weak when key B is set to 0. Choose different key');
end;
end;
  if ((key_a < 0) or (key_b < 0)) or (key_b > (check_keys_m - 1)) then begin
  panic('Key A must be greater than 0 and key B must be between 0 and ' + IntToStr(check_keys_m - 1));
end;
  if gcd(key_a, check_keys_m) <> 1 then begin
  panic(((('Key A ' + IntToStr(key_a)) + ' and the symbol set size ') + IntToStr(check_keys_m)) + ' are not relatively prime. Choose a different key.');
end;
end;
function encrypt_message(key: integer; message: string): string;
var
  encrypt_message_m: integer;
  encrypt_message_key_a: integer;
  encrypt_message_key_b: integer;
  encrypt_message_cipher_text: string;
  encrypt_message_i: integer;
  encrypt_message_ch: string;
  encrypt_message_index: integer;
begin
  encrypt_message_m := Length(SYMBOLS);
  encrypt_message_key_a := key div encrypt_message_m;
  encrypt_message_key_b := key mod encrypt_message_m;
  check_keys(encrypt_message_key_a, encrypt_message_key_b, 'encrypt');
  encrypt_message_cipher_text := '';
  encrypt_message_i := 0;
  while encrypt_message_i < Length(message) do begin
  encrypt_message_ch := message[encrypt_message_i+1];
  encrypt_message_index := find_symbol(encrypt_message_ch);
  if encrypt_message_index >= 0 then begin
  encrypt_message_cipher_text := encrypt_message_cipher_text + SYMBOLS[((encrypt_message_index * encrypt_message_key_a) + encrypt_message_key_b) mod encrypt_message_m+1];
end else begin
  encrypt_message_cipher_text := encrypt_message_cipher_text + encrypt_message_ch;
end;
  encrypt_message_i := encrypt_message_i + 1;
end;
  exit(encrypt_message_cipher_text);
end;
function decrypt_message(key: integer; message: string): string;
var
  decrypt_message_m: integer;
  decrypt_message_key_a: integer;
  decrypt_message_key_b: integer;
  decrypt_message_inv: integer;
  decrypt_message_plain_text: string;
  decrypt_message_i: integer;
  decrypt_message_ch: string;
  decrypt_message_index: integer;
  decrypt_message_n: integer;
  decrypt_message_pos: integer;
  decrypt_message_final: integer;
begin
  decrypt_message_m := Length(SYMBOLS);
  decrypt_message_key_a := key div decrypt_message_m;
  decrypt_message_key_b := key mod decrypt_message_m;
  check_keys(decrypt_message_key_a, decrypt_message_key_b, 'decrypt');
  decrypt_message_inv := mod_inverse(decrypt_message_key_a, decrypt_message_m);
  decrypt_message_plain_text := '';
  decrypt_message_i := 0;
  while decrypt_message_i < Length(message) do begin
  decrypt_message_ch := message[decrypt_message_i+1];
  decrypt_message_index := find_symbol(decrypt_message_ch);
  if decrypt_message_index >= 0 then begin
  decrypt_message_n := (decrypt_message_index - decrypt_message_key_b) * decrypt_message_inv;
  decrypt_message_pos := decrypt_message_n mod decrypt_message_m;
  decrypt_message_final := IfThen(decrypt_message_pos < 0, decrypt_message_pos + decrypt_message_m, decrypt_message_pos);
  decrypt_message_plain_text := decrypt_message_plain_text + SYMBOLS[decrypt_message_final+1];
end else begin
  decrypt_message_plain_text := decrypt_message_plain_text + decrypt_message_ch;
end;
  decrypt_message_i := decrypt_message_i + 1;
end;
  exit(decrypt_message_plain_text);
end;
procedure main();
var
  main_key: integer;
  main_msg: string;
  main_enc: string;
begin
  main_key := 4545;
  main_msg := 'The affine cipher is a type of monoalphabetic substitution cipher.';
  main_enc := encrypt_message(main_key, main_msg);
  writeln(main_enc);
  writeln(decrypt_message(main_key, main_enc));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  SYMBOLS := ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
