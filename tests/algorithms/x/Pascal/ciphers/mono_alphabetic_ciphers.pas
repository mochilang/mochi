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
  LETTERS: string;
  s: string;
  message: string;
  ch: string;
  key: string;
function find_char(s: string; ch: string): integer; forward;
function encrypt_message(key: string; message: string): string; forward;
function decrypt_message(key: string; message: string): string; forward;
procedure main(); forward;
function find_char(s: string; ch: string): integer;
var
  find_char_i: integer;
begin
  find_char_i := 0;
  while find_char_i < Length(s) do begin
  if s[find_char_i+1] = ch then begin
  exit(find_char_i);
end;
  find_char_i := find_char_i + 1;
end;
  exit(-1);
end;
function encrypt_message(key: string; message: string): string;
var
  encrypt_message_chars_a: string;
  encrypt_message_chars_b: string;
  encrypt_message_translated: string;
  encrypt_message_i: integer;
  encrypt_message_symbol: string;
  encrypt_message_upper_sym: string;
  encrypt_message_sym_index: integer;
  encrypt_message_sub_char: string;
begin
  encrypt_message_chars_a := key;
  encrypt_message_chars_b := LETTERS;
  encrypt_message_translated := '';
  encrypt_message_i := 0;
  while encrypt_message_i < Length(message) do begin
  encrypt_message_symbol := message[encrypt_message_i+1];
  encrypt_message_upper_sym := UpperCase(encrypt_message_symbol);
  encrypt_message_sym_index := find_char(encrypt_message_chars_a, encrypt_message_upper_sym);
  if encrypt_message_sym_index >= 0 then begin
  encrypt_message_sub_char := encrypt_message_chars_b[encrypt_message_sym_index+1];
  if encrypt_message_symbol = encrypt_message_upper_sym then begin
  encrypt_message_translated := encrypt_message_translated + UpperCase(encrypt_message_sub_char);
end else begin
  encrypt_message_translated := encrypt_message_translated + LowerCase(encrypt_message_sub_char);
end;
end else begin
  encrypt_message_translated := encrypt_message_translated + encrypt_message_symbol;
end;
  encrypt_message_i := encrypt_message_i + 1;
end;
  exit(encrypt_message_translated);
end;
function decrypt_message(key: string; message: string): string;
var
  decrypt_message_chars_a: string;
  decrypt_message_chars_b: string;
  decrypt_message_translated: string;
  decrypt_message_i: integer;
  decrypt_message_symbol: string;
  decrypt_message_upper_sym: string;
  decrypt_message_sym_index: integer;
  decrypt_message_sub_char: string;
begin
  decrypt_message_chars_a := LETTERS;
  decrypt_message_chars_b := key;
  decrypt_message_translated := '';
  decrypt_message_i := 0;
  while decrypt_message_i < Length(message) do begin
  decrypt_message_symbol := message[decrypt_message_i+1];
  decrypt_message_upper_sym := UpperCase(decrypt_message_symbol);
  decrypt_message_sym_index := find_char(decrypt_message_chars_a, decrypt_message_upper_sym);
  if decrypt_message_sym_index >= 0 then begin
  decrypt_message_sub_char := decrypt_message_chars_b[decrypt_message_sym_index+1];
  if decrypt_message_symbol = decrypt_message_upper_sym then begin
  decrypt_message_translated := decrypt_message_translated + UpperCase(decrypt_message_sub_char);
end else begin
  decrypt_message_translated := decrypt_message_translated + LowerCase(decrypt_message_sub_char);
end;
end else begin
  decrypt_message_translated := decrypt_message_translated + decrypt_message_symbol;
end;
  decrypt_message_i := decrypt_message_i + 1;
end;
  exit(decrypt_message_translated);
end;
procedure main();
var
  main_message: string;
  main_key: string;
  main_mode: string;
  main_translated: string;
begin
  main_message := 'Hello World';
  main_key := 'QWERTYUIOPASDFGHJKLZXCVBNM';
  main_mode := 'decrypt';
  main_translated := '';
  if main_mode = 'encrypt' then begin
  main_translated := encrypt_message(main_key, main_message);
end else begin
  if main_mode = 'decrypt' then begin
  main_translated := decrypt_message(main_key, main_message);
end;
end;
  writeln((((('Using the key ' + main_key) + ', the ') + main_mode) + 'ed message is: ') + main_translated);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LETTERS := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
