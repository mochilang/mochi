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
  key: string;
  plaintext: string;
  ciphertext: string;
  ch: string;
  n: integer;
  s: string;
function indexOf(s: string; ch: string): integer; forward;
function ord(ch: string): integer; forward;
function chr(n: integer): string; forward;
function clean_text(s: string): string; forward;
function running_key_encrypt(key: string; plaintext: string): string; forward;
function running_key_decrypt(key: string; ciphertext: string): string; forward;
function indexOf(s: string; ch: string): integer;
var
  indexOf_i: integer;
begin
  indexOf_i := 0;
  while indexOf_i < Length(s) do begin
  if s[indexOf_i+1] = ch then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(-1);
end;
function ord(ch: string): integer;
var
  ord_upper: string;
  ord_lower: string;
  ord_idx: integer;
begin
  ord_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ord_lower := 'abcdefghijklmnopqrstuvwxyz';
  ord_idx := indexOf(ord_upper, ch);
  if ord_idx >= 0 then begin
  exit(65 + ord_idx);
end;
  ord_idx := indexOf(ord_lower, ch);
  if ord_idx >= 0 then begin
  exit(97 + ord_idx);
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
function clean_text(s: string): string;
var
  clean_text_out: string;
  clean_text_i: integer;
  clean_text_ch: string;
begin
  clean_text_out := '';
  clean_text_i := 0;
  while clean_text_i < Length(s) do begin
  clean_text_ch := s[clean_text_i+1];
  if (clean_text_ch >= 'A') and (clean_text_ch <= 'Z') then begin
  clean_text_out := clean_text_out + clean_text_ch;
end else begin
  if (clean_text_ch >= 'a') and (clean_text_ch <= 'z') then begin
  clean_text_out := clean_text_out + chr(ord(clean_text_ch) - 32);
end;
end;
  clean_text_i := clean_text_i + 1;
end;
  exit(clean_text_out);
end;
function running_key_encrypt(key: string; plaintext: string): string;
var
  running_key_encrypt_pt: string;
  running_key_encrypt_k: string;
  running_key_encrypt_key_len: integer;
  running_key_encrypt_res: string;
  running_key_encrypt_ord_a: integer;
  running_key_encrypt_i: integer;
  running_key_encrypt_p: integer;
  running_key_encrypt_kv: integer;
  running_key_encrypt_c: integer;
begin
  running_key_encrypt_pt := clean_text(plaintext);
  running_key_encrypt_k := clean_text(key);
  running_key_encrypt_key_len := Length(running_key_encrypt_k);
  running_key_encrypt_res := '';
  running_key_encrypt_ord_a := ord('A');
  running_key_encrypt_i := 0;
  while running_key_encrypt_i < Length(running_key_encrypt_pt) do begin
  running_key_encrypt_p := ord(running_key_encrypt_pt[running_key_encrypt_i+1]) - running_key_encrypt_ord_a;
  running_key_encrypt_kv := ord(running_key_encrypt_k[running_key_encrypt_i mod running_key_encrypt_key_len+1]) - running_key_encrypt_ord_a;
  running_key_encrypt_c := (running_key_encrypt_p + running_key_encrypt_kv) mod 26;
  running_key_encrypt_res := running_key_encrypt_res + chr(running_key_encrypt_c + running_key_encrypt_ord_a);
  running_key_encrypt_i := running_key_encrypt_i + 1;
end;
  exit(running_key_encrypt_res);
end;
function running_key_decrypt(key: string; ciphertext: string): string;
var
  running_key_decrypt_ct: string;
  running_key_decrypt_k: string;
  running_key_decrypt_key_len: integer;
  running_key_decrypt_res: string;
  running_key_decrypt_ord_a: integer;
  running_key_decrypt_i: integer;
  running_key_decrypt_c: integer;
  running_key_decrypt_kv: integer;
  running_key_decrypt_p: integer;
begin
  running_key_decrypt_ct := clean_text(ciphertext);
  running_key_decrypt_k := clean_text(key);
  running_key_decrypt_key_len := Length(running_key_decrypt_k);
  running_key_decrypt_res := '';
  running_key_decrypt_ord_a := ord('A');
  running_key_decrypt_i := 0;
  while running_key_decrypt_i < Length(running_key_decrypt_ct) do begin
  running_key_decrypt_c := ord(running_key_decrypt_ct[running_key_decrypt_i+1]) - running_key_decrypt_ord_a;
  running_key_decrypt_kv := ord(running_key_decrypt_k[running_key_decrypt_i mod running_key_decrypt_key_len+1]) - running_key_decrypt_ord_a;
  running_key_decrypt_p := ((running_key_decrypt_c - running_key_decrypt_kv) + 26) mod 26;
  running_key_decrypt_res := running_key_decrypt_res + chr(running_key_decrypt_p + running_key_decrypt_ord_a);
  running_key_decrypt_i := running_key_decrypt_i + 1;
end;
  exit(running_key_decrypt_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  key := 'How does the duck know that? said Victor';
  plaintext := 'DEFEND THIS';
  ciphertext := running_key_encrypt(key, plaintext);
  writeln(ciphertext);
  writeln(running_key_decrypt(key, ciphertext));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
