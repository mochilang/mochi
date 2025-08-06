{$mode objfpc}
program Main;
uses SysUtils, fgl;
type IntArray = array of integer;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
  ascii_chars: string;
  res: specialize TFPGMap<string, IntArray>;
  cipher: integer;
  cipher_idx: integer;
  key: integer;
  key_idx: integer;
  b: integer;
  ch: string;
  a: integer;
  s: integer;
  code: integer;
  text: string;
procedure set_seed(s: integer); forward;
function randint(a: integer; b: integer): integer; forward;
function ord(ch: string): integer; forward;
function chr(code: integer): string; forward;
function encrypt(text: string): specialize TFPGMap<string, IntArray>; forward;
function decrypt(cipher: IntArray; key: IntArray): string; forward;
procedure set_seed(s: integer);
begin
  seed := s;
end;
function randint(a: integer; b: integer): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit((seed mod ((b - a) + 1)) + a);
end;
function ord(ch: string): integer;
var
  ord_i: integer;
begin
  ord_i := 0;
  while ord_i < Length(ascii_chars) do begin
  if ascii_chars[ord_i+1] = ch then begin
  exit(32 + ord_i);
end;
  ord_i := ord_i + 1;
end;
  exit(0);
end;
function chr(code: integer): string;
begin
  if (code < 32) or (code > 126) then begin
  exit('');
end;
  exit(ascii_chars[code - 32+1]);
end;
function encrypt(text: string): specialize TFPGMap<string, IntArray>;
var
  encrypt_cipher: array of integer;
  encrypt_key: array of integer;
  encrypt_i: integer;
  encrypt_p: integer;
  encrypt_k: integer;
  encrypt_c: integer;
  encrypt_res: specialize TFPGMap<string, IntArray>;
begin
  encrypt_cipher := [];
  encrypt_key := [];
  encrypt_i := 0;
  while encrypt_i < Length(text) do begin
  encrypt_p := ord(text[encrypt_i+1]);
  encrypt_k := randint(1, 300);
  encrypt_c := (encrypt_p + encrypt_k) * encrypt_k;
  encrypt_cipher := concat(encrypt_cipher, IntArray([encrypt_c]));
  encrypt_key := concat(encrypt_key, IntArray([encrypt_k]));
  encrypt_i := encrypt_i + 1;
end;
  encrypt_res := specialize TFPGMap<string, IntArray>.Create();
  encrypt_res.AddOrSetData('cipher', Variant(PtrUInt(encrypt_cipher)));
  encrypt_res.AddOrSetData('key', Variant(PtrUInt(encrypt_key)));
  exit(encrypt_res);
end;
function decrypt(cipher: IntArray; key: IntArray): string;
var
  decrypt_plain: string;
  decrypt_i: integer;
  decrypt_p: integer;
begin
  decrypt_plain := '';
  decrypt_i := 0;
  while decrypt_i < Length(key) do begin
  decrypt_p := (cipher[decrypt_i] - (key[decrypt_i] * key[decrypt_i])) div key[decrypt_i];
  decrypt_plain := decrypt_plain + chr(decrypt_p);
  decrypt_i := decrypt_i + 1;
end;
  exit(decrypt_plain);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  ascii_chars := ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  set_seed(1);
  res := encrypt('Hello');
  cipher_idx := res.IndexOf('cipher');
  if cipher_idx <> -1 then begin
  cipher := res.Data[cipher_idx];
end else begin
  cipher := 0;
end;
  key_idx := res.IndexOf('key');
  if key_idx <> -1 then begin
  key := res.Data[key_idx];
end else begin
  key := 0;
end;
  show_list(cipher);
  show_list(key);
  writeln(decrypt(cipher, key));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
