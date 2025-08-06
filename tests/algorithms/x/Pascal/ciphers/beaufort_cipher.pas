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
  ALPHABET: string;
  message: string;
  key: string;
  key_new: string;
  encrypted: string;
  ch: string;
  cipher: string;
function index_of(ch: string): integer; forward;
function generate_key(message: string; key: string): string; forward;
function cipher_text(message: string; key_new: string): string; forward;
function original_text(cipher: string; key_new: string): string; forward;
function index_of(ch: string): integer;
var
  index_of_i: integer;
begin
  for index_of_i := 0 to (Length(ALPHABET) - 1) do begin
  if ALPHABET[index_of_i+1] = ch then begin
  exit(index_of_i);
end;
end;
  exit(-1);
end;
function generate_key(message: string; key: string): string;
var
  generate_key_key_new: string;
  generate_key_i: integer;
begin
  generate_key_key_new := key;
  generate_key_i := 0;
  while Length(generate_key_key_new) < Length(message) do begin
  generate_key_key_new := generate_key_key_new + key[generate_key_i+1];
  generate_key_i := generate_key_i + 1;
  if generate_key_i = Length(key) then begin
  generate_key_i := 0;
end;
end;
  exit(generate_key_key_new);
end;
function cipher_text(message: string; key_new: string): string;
var
  cipher_text_res: string;
  cipher_text_i: integer;
  cipher_text_idx: integer;
  cipher_text_ch: string;
  cipher_text_x: integer;
begin
  cipher_text_res := '';
  cipher_text_i := 0;
  for cipher_text_idx := 0 to (Length(message) - 1) do begin
  cipher_text_ch := message[cipher_text_idx+1];
  if cipher_text_ch = ' ' then begin
  cipher_text_res := cipher_text_res + ' ';
end else begin
  cipher_text_x := ((index_of(cipher_text_ch) - index_of(key_new[cipher_text_i+1])) + 26) mod 26;
  cipher_text_i := cipher_text_i + 1;
  cipher_text_res := cipher_text_res + ALPHABET[cipher_text_x+1];
end;
end;
  exit(cipher_text_res);
end;
function original_text(cipher: string; key_new: string): string;
var
  original_text_res: string;
  original_text_i: integer;
  original_text_idx: integer;
  original_text_ch: string;
  original_text_x: integer;
begin
  original_text_res := '';
  original_text_i := 0;
  for original_text_idx := 0 to (Length(cipher) - 1) do begin
  original_text_ch := cipher[original_text_idx+1];
  if original_text_ch = ' ' then begin
  original_text_res := original_text_res + ' ';
end else begin
  original_text_x := ((index_of(original_text_ch) + index_of(key_new[original_text_i+1])) + 26) mod 26;
  original_text_i := original_text_i + 1;
  original_text_res := original_text_res + ALPHABET[original_text_x+1];
end;
end;
  exit(original_text_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ALPHABET := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  message := 'THE GERMAN ATTACK';
  key := 'SECRET';
  key_new := generate_key(message, key);
  encrypted := cipher_text(message, key_new);
  writeln('Encrypted Text = ' + encrypted);
  writeln('Original Text = ' + original_text(encrypted, key_new));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
