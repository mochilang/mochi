{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  MORSE_CODE_DICT: specialize TFPGMap<string, string>;
  MORSE_COMBINATIONS: array of string;
  REVERSE_DICT: specialize TFPGMap<string, string>;
  plaintext: string;
  key: string;
  ciphertext: string;
  decrypted: string;
function Map2(): specialize TFPGMap<string, string>; forward;
function Map1(): specialize TFPGMap<string, string>; forward;
function encodeToMorse(plaintext: string): string; forward;
function encryptFractionatedMorse(plaintext: string; key: string): string; forward;
function decryptFractionatedMorse(ciphertext: string; key: string): string; forward;
function Map2(): specialize TFPGMap<string, string>;
begin
  Result := specialize TFPGMap<string, string>.Create();
  Result.AddOrSetData('.-', Variant('A'));
  Result.AddOrSetData('-...', Variant('B'));
  Result.AddOrSetData('-.-.', Variant('C'));
  Result.AddOrSetData('-..', Variant('D'));
  Result.AddOrSetData('.', Variant('E'));
  Result.AddOrSetData('..-.', Variant('F'));
  Result.AddOrSetData('--.', Variant('G'));
  Result.AddOrSetData('....', Variant('H'));
  Result.AddOrSetData('..', Variant('I'));
  Result.AddOrSetData('.---', Variant('J'));
  Result.AddOrSetData('-.-', Variant('K'));
  Result.AddOrSetData('.-..', Variant('L'));
  Result.AddOrSetData('--', Variant('M'));
  Result.AddOrSetData('-.', Variant('N'));
  Result.AddOrSetData('---', Variant('O'));
  Result.AddOrSetData('.--.', Variant('P'));
  Result.AddOrSetData('--.-', Variant('Q'));
  Result.AddOrSetData('.-.', Variant('R'));
  Result.AddOrSetData('...', Variant('S'));
  Result.AddOrSetData('-', Variant('T'));
  Result.AddOrSetData('..-', Variant('U'));
  Result.AddOrSetData('...-', Variant('V'));
  Result.AddOrSetData('.--', Variant('W'));
  Result.AddOrSetData('-..-', Variant('X'));
  Result.AddOrSetData('-.--', Variant('Y'));
  Result.AddOrSetData('--..', Variant('Z'));
  Result.AddOrSetData('', Variant(' '));
end;
function Map1(): specialize TFPGMap<string, string>;
begin
  Result := specialize TFPGMap<string, string>.Create();
  Result.AddOrSetData('A', Variant('.-'));
  Result.AddOrSetData('B', Variant('-...'));
  Result.AddOrSetData('C', Variant('-.-.'));
  Result.AddOrSetData('D', Variant('-..'));
  Result.AddOrSetData('E', Variant('.'));
  Result.AddOrSetData('F', Variant('..-.'));
  Result.AddOrSetData('G', Variant('--.'));
  Result.AddOrSetData('H', Variant('....'));
  Result.AddOrSetData('I', Variant('..'));
  Result.AddOrSetData('J', Variant('.---'));
  Result.AddOrSetData('K', Variant('-.-'));
  Result.AddOrSetData('L', Variant('.-..'));
  Result.AddOrSetData('M', Variant('--'));
  Result.AddOrSetData('N', Variant('-.'));
  Result.AddOrSetData('O', Variant('---'));
  Result.AddOrSetData('P', Variant('.--.'));
  Result.AddOrSetData('Q', Variant('--.-'));
  Result.AddOrSetData('R', Variant('.-.'));
  Result.AddOrSetData('S', Variant('...'));
  Result.AddOrSetData('T', Variant('-'));
  Result.AddOrSetData('U', Variant('..-'));
  Result.AddOrSetData('V', Variant('...-'));
  Result.AddOrSetData('W', Variant('.--'));
  Result.AddOrSetData('X', Variant('-..-'));
  Result.AddOrSetData('Y', Variant('-.--'));
  Result.AddOrSetData('Z', Variant('--..'));
  Result.AddOrSetData(' ', Variant(''));
end;
function encodeToMorse(plaintext: string): string;
var
  encodeToMorse_morse: string;
  encodeToMorse_i: integer;
  encodeToMorse_ch: string;
  encodeToMorse_code: string;
begin
  encodeToMorse_morse := '';
  encodeToMorse_i := 0;
  while encodeToMorse_i < Length(plaintext) do begin
  encodeToMorse_ch := UpperCase(copy(plaintext, encodeToMorse_i+1, (encodeToMorse_i + 1 - (encodeToMorse_i))));
  encodeToMorse_code := '';
  if MORSE_CODE_DICT.IndexOf(encodeToMorse_ch) <> -1 then begin
  encodeToMorse_code := MORSE_CODE_DICT[encodeToMorse_ch];
end;
  if encodeToMorse_i > 0 then begin
  encodeToMorse_morse := encodeToMorse_morse + 'x';
end;
  encodeToMorse_morse := encodeToMorse_morse + encodeToMorse_code;
  encodeToMorse_i := encodeToMorse_i + 1;
end;
  exit(encodeToMorse_morse);
end;
function encryptFractionatedMorse(plaintext: string; key: string): string;
var
  encryptFractionatedMorse_morseCode: string;
  encryptFractionatedMorse_combinedKey: string;
  encryptFractionatedMorse_dedupKey: string;
  encryptFractionatedMorse_i: integer;
  encryptFractionatedMorse_ch: string;
  encryptFractionatedMorse_paddingLength: integer;
  encryptFractionatedMorse_p: integer;
  encryptFractionatedMorse_dict: specialize TFPGMap<string, string>;
  encryptFractionatedMorse_j: integer;
  encryptFractionatedMorse_combo: string;
  encryptFractionatedMorse_letter: string;
  encryptFractionatedMorse_encrypted: string;
  encryptFractionatedMorse_k: integer;
  encryptFractionatedMorse_group: string;
begin
  encryptFractionatedMorse_morseCode := encodeToMorse(plaintext);
  encryptFractionatedMorse_combinedKey := UpperCase(key) + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  encryptFractionatedMorse_dedupKey := '';
  encryptFractionatedMorse_i := 0;
  while encryptFractionatedMorse_i < Length(encryptFractionatedMorse_combinedKey) do begin
  encryptFractionatedMorse_ch := copy(encryptFractionatedMorse_combinedKey, encryptFractionatedMorse_i+1, (encryptFractionatedMorse_i + 1 - (encryptFractionatedMorse_i)));
  if not (Pos(encryptFractionatedMorse_ch, encryptFractionatedMorse_dedupKey) <> 0) then begin
  encryptFractionatedMorse_dedupKey := encryptFractionatedMorse_dedupKey + encryptFractionatedMorse_ch;
end;
  encryptFractionatedMorse_i := encryptFractionatedMorse_i + 1;
end;
  encryptFractionatedMorse_paddingLength := 3 - (Length(encryptFractionatedMorse_morseCode) mod 3);
  encryptFractionatedMorse_p := 0;
  while encryptFractionatedMorse_p < encryptFractionatedMorse_paddingLength do begin
  encryptFractionatedMorse_morseCode := encryptFractionatedMorse_morseCode + 'x';
  encryptFractionatedMorse_p := encryptFractionatedMorse_p + 1;
end;
  encryptFractionatedMorse_dict := specialize TFPGMap<string, string>.Create();
  encryptFractionatedMorse_j := 0;
  while encryptFractionatedMorse_j < 26 do begin
  encryptFractionatedMorse_combo := MORSE_COMBINATIONS[encryptFractionatedMorse_j];
  encryptFractionatedMorse_letter := copy(encryptFractionatedMorse_dedupKey, encryptFractionatedMorse_j+1, (encryptFractionatedMorse_j + 1 - (encryptFractionatedMorse_j)));
  encryptFractionatedMorse_dict.AddOrSetData(encryptFractionatedMorse_combo, Variant(encryptFractionatedMorse_letter));
  encryptFractionatedMorse_j := encryptFractionatedMorse_j + 1;
end;
  encryptFractionatedMorse_dict.AddOrSetData('xxx', Variant(''));
  encryptFractionatedMorse_encrypted := '';
  encryptFractionatedMorse_k := 0;
  while encryptFractionatedMorse_k < Length(encryptFractionatedMorse_morseCode) do begin
  encryptFractionatedMorse_group := copy(encryptFractionatedMorse_morseCode, encryptFractionatedMorse_k+1, (encryptFractionatedMorse_k + 3 - (encryptFractionatedMorse_k)));
  encryptFractionatedMorse_encrypted := encryptFractionatedMorse_encrypted + encryptFractionatedMorse_dict[encryptFractionatedMorse_group];
  encryptFractionatedMorse_k := encryptFractionatedMorse_k + 3;
end;
  exit(encryptFractionatedMorse_encrypted);
end;
function decryptFractionatedMorse(ciphertext: string; key: string): string;
var
  decryptFractionatedMorse_combinedKey: string;
  decryptFractionatedMorse_dedupKey: string;
  decryptFractionatedMorse_i: integer;
  decryptFractionatedMorse_ch: string;
  decryptFractionatedMorse_inv: specialize TFPGMap<string, string>;
  decryptFractionatedMorse_j: integer;
  decryptFractionatedMorse_letter: string;
  decryptFractionatedMorse_morse: string;
  decryptFractionatedMorse_k: integer;
  decryptFractionatedMorse_codes: array of string;
  decryptFractionatedMorse_current: string;
  decryptFractionatedMorse_m: integer;
  decryptFractionatedMorse_decrypted: string;
  decryptFractionatedMorse_idx: integer;
  decryptFractionatedMorse_code: string;
  decryptFractionatedMorse_start: integer;
  decryptFractionatedMorse_end: integer;
begin
  decryptFractionatedMorse_combinedKey := UpperCase(key) + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  decryptFractionatedMorse_dedupKey := '';
  decryptFractionatedMorse_i := 0;
  while decryptFractionatedMorse_i < Length(decryptFractionatedMorse_combinedKey) do begin
  decryptFractionatedMorse_ch := copy(decryptFractionatedMorse_combinedKey, decryptFractionatedMorse_i+1, (decryptFractionatedMorse_i + 1 - (decryptFractionatedMorse_i)));
  if not (Pos(decryptFractionatedMorse_ch, decryptFractionatedMorse_dedupKey) <> 0) then begin
  decryptFractionatedMorse_dedupKey := decryptFractionatedMorse_dedupKey + decryptFractionatedMorse_ch;
end;
  decryptFractionatedMorse_i := decryptFractionatedMorse_i + 1;
end;
  decryptFractionatedMorse_inv := specialize TFPGMap<string, string>.Create();
  decryptFractionatedMorse_j := 0;
  while decryptFractionatedMorse_j < 26 do begin
  decryptFractionatedMorse_letter := copy(decryptFractionatedMorse_dedupKey, decryptFractionatedMorse_j+1, (decryptFractionatedMorse_j + 1 - (decryptFractionatedMorse_j)));
  decryptFractionatedMorse_inv.AddOrSetData(decryptFractionatedMorse_letter, Variant(MORSE_COMBINATIONS[decryptFractionatedMorse_j]));
  decryptFractionatedMorse_j := decryptFractionatedMorse_j + 1;
end;
  decryptFractionatedMorse_morse := '';
  decryptFractionatedMorse_k := 0;
  while decryptFractionatedMorse_k < Length(ciphertext) do begin
  decryptFractionatedMorse_ch := copy(ciphertext, decryptFractionatedMorse_k+1, (decryptFractionatedMorse_k + 1 - (decryptFractionatedMorse_k)));
  if decryptFractionatedMorse_inv.IndexOf(decryptFractionatedMorse_ch) <> -1 then begin
  decryptFractionatedMorse_morse := decryptFractionatedMorse_morse + decryptFractionatedMorse_inv[decryptFractionatedMorse_ch];
end;
  decryptFractionatedMorse_k := decryptFractionatedMorse_k + 1;
end;
  decryptFractionatedMorse_codes := [];
  decryptFractionatedMorse_current := '';
  decryptFractionatedMorse_m := 0;
  while decryptFractionatedMorse_m < Length(decryptFractionatedMorse_morse) do begin
  decryptFractionatedMorse_ch := copy(decryptFractionatedMorse_morse, decryptFractionatedMorse_m+1, (decryptFractionatedMorse_m + 1 - (decryptFractionatedMorse_m)));
  if decryptFractionatedMorse_ch = 'x' then begin
  decryptFractionatedMorse_codes := concat(decryptFractionatedMorse_codes, StrArray([decryptFractionatedMorse_current]));
  decryptFractionatedMorse_current := '';
end else begin
  decryptFractionatedMorse_current := decryptFractionatedMorse_current + decryptFractionatedMorse_ch;
end;
  decryptFractionatedMorse_m := decryptFractionatedMorse_m + 1;
end;
  decryptFractionatedMorse_codes := concat(decryptFractionatedMorse_codes, StrArray([decryptFractionatedMorse_current]));
  decryptFractionatedMorse_decrypted := '';
  decryptFractionatedMorse_idx := 0;
  while decryptFractionatedMorse_idx < Length(decryptFractionatedMorse_codes) do begin
  decryptFractionatedMorse_code := decryptFractionatedMorse_codes[decryptFractionatedMorse_idx];
  decryptFractionatedMorse_decrypted := decryptFractionatedMorse_decrypted + REVERSE_DICT[decryptFractionatedMorse_code];
  decryptFractionatedMorse_idx := decryptFractionatedMorse_idx + 1;
end;
  decryptFractionatedMorse_start := 0;
  while true do begin
  if decryptFractionatedMorse_start < Length(decryptFractionatedMorse_decrypted) then begin
  if copy(decryptFractionatedMorse_decrypted, decryptFractionatedMorse_start+1, (decryptFractionatedMorse_start + 1 - (decryptFractionatedMorse_start))) = ' ' then begin
  decryptFractionatedMorse_start := decryptFractionatedMorse_start + 1;
  continue;
end;
end;
  break;
end;
  decryptFractionatedMorse_end := Length(decryptFractionatedMorse_decrypted);
  while true do begin
  if decryptFractionatedMorse_end > decryptFractionatedMorse_start then begin
  if copy(decryptFractionatedMorse_decrypted, decryptFractionatedMorse_end - 1+1, (decryptFractionatedMorse_end - (decryptFractionatedMorse_end - 1))) = ' ' then begin
  decryptFractionatedMorse_end := decryptFractionatedMorse_end - 1;
  continue;
end;
end;
  break;
end;
  exit(copy(decryptFractionatedMorse_decrypted, decryptFractionatedMorse_start+1, (decryptFractionatedMorse_end - (decryptFractionatedMorse_start))));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  MORSE_CODE_DICT := Map1();
  MORSE_COMBINATIONS := ['...', '..-', '..x', '.-.', '.--', '.-x', '.x.', '.x-', '.xx', '-..', '-.-', '-.x', '--.', '---', '--x', '-x.', '-x-', '-xx', 'x..', 'x.-', 'x.x', 'x-.', 'x--', 'x-x', 'xx.', 'xx-', 'xxx'];
  REVERSE_DICT := Map2();
  plaintext := 'defend the east';
  writeln('Plain Text:', ' ', plaintext);
  key := 'ROUNDTABLE';
  ciphertext := encryptFractionatedMorse(plaintext, key);
  writeln('Encrypted:', ' ', ciphertext);
  decrypted := decryptFractionatedMorse(ciphertext, key);
  writeln('Decrypted:', ' ', decrypted);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
