{$mode objfpc}
program Main;
uses SysUtils, StrUtils, fgl;
type Result = record
  shift: integer;
  chi: real;
  decoded: string;
end;
type StrArray = array of string;
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
  r1: Result;
  r2: Result;
  r3: Result;
  xs: StrArray;
  ch: string;
  frequencies_dict: specialize TFPGMap<string, real>;
  s: string;
  case_sensitive: boolean;
  ciphertext: string;
  cipher_alphabet: StrArray;
function Map2(): specialize TFPGMap<string, integer>; forward;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function makeResult(shift: integer; chi: real; decoded: string): Result; forward;
function default_alphabet(): StrArray; forward;
function default_frequencies(): specialize TFPGMap<string, Variant>; forward;
function index_of(xs: StrArray; ch: string): integer; forward;
function count_char(s: string; ch: string): integer; forward;
function decrypt_caesar_with_chi_squared(ciphertext: string; cipher_alphabet: StrArray; frequencies_dict: specialize TFPGMap<string, real>; case_sensitive: boolean): Result; forward;
function Map2(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
end;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('a', Variant(0.08497));
  Result.AddOrSetData('b', Variant(0.01492));
  Result.AddOrSetData('c', Variant(0.02202));
  Result.AddOrSetData('d', Variant(0.04253));
  Result.AddOrSetData('e', Variant(0.11162));
  Result.AddOrSetData('f', Variant(0.02228));
  Result.AddOrSetData('g', Variant(0.02015));
  Result.AddOrSetData('h', Variant(0.06094));
  Result.AddOrSetData('i', Variant(0.07546));
  Result.AddOrSetData('j', Variant(0.00153));
  Result.AddOrSetData('k', Variant(0.01292));
  Result.AddOrSetData('l', Variant(0.04025));
  Result.AddOrSetData('m', Variant(0.02406));
  Result.AddOrSetData('n', Variant(0.06749));
  Result.AddOrSetData('o', Variant(0.07507));
  Result.AddOrSetData('p', Variant(0.01929));
  Result.AddOrSetData('q', Variant(0.00095));
  Result.AddOrSetData('r', Variant(0.07587));
  Result.AddOrSetData('s', Variant(0.06327));
  Result.AddOrSetData('t', Variant(0.09356));
  Result.AddOrSetData('u', Variant(0.02758));
  Result.AddOrSetData('v', Variant(0.00978));
  Result.AddOrSetData('w', Variant(0.0256));
  Result.AddOrSetData('x', Variant(0.0015));
  Result.AddOrSetData('y', Variant(0.01994));
  Result.AddOrSetData('z', Variant(0.00077));
end;
function makeResult(shift: integer; chi: real; decoded: string): Result;
begin
  Result.shift := shift;
  Result.chi := chi;
  Result.decoded := decoded;
end;
function default_alphabet(): StrArray;
begin
  exit(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']);
end;
function default_frequencies(): specialize TFPGMap<string, Variant>;
begin
  exit(Map1());
end;
function index_of(xs: StrArray; ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(xs) do begin
  if xs[index_of_i] = ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function count_char(s: string; ch: string): integer;
var
  count_char_count: integer;
  count_char_i: integer;
begin
  count_char_count := 0;
  count_char_i := 0;
  while count_char_i < Length(s) do begin
  if copy(s, count_char_i+1, (count_char_i + 1 - (count_char_i))) = ch then begin
  count_char_count := count_char_count + 1;
end;
  count_char_i := count_char_i + 1;
end;
  exit(count_char_count);
end;
function decrypt_caesar_with_chi_squared(ciphertext: string; cipher_alphabet: StrArray; frequencies_dict: specialize TFPGMap<string, real>; case_sensitive: boolean): Result;
var
  decrypt_caesar_with_chi_squared_alphabet_letters: array of string;
  decrypt_caesar_with_chi_squared_frequencies: specialize TFPGMap<string, real>;
  decrypt_caesar_with_chi_squared_best_shift: integer;
  decrypt_caesar_with_chi_squared_best_chi: real;
  decrypt_caesar_with_chi_squared_best_text: string;
  decrypt_caesar_with_chi_squared_shift: integer;
  decrypt_caesar_with_chi_squared_decrypted: string;
  decrypt_caesar_with_chi_squared_i: integer;
  decrypt_caesar_with_chi_squared_ch: string;
  decrypt_caesar_with_chi_squared_idx: integer;
  decrypt_caesar_with_chi_squared_m: integer;
  decrypt_caesar_with_chi_squared_new_idx: integer;
  decrypt_caesar_with_chi_squared_new_char: string;
  decrypt_caesar_with_chi_squared_chi: real;
  decrypt_caesar_with_chi_squared_lowered: string;
  decrypt_caesar_with_chi_squared_j: integer;
  decrypt_caesar_with_chi_squared_letter: string;
  decrypt_caesar_with_chi_squared_occ: integer;
  decrypt_caesar_with_chi_squared_occf: real;
  decrypt_caesar_with_chi_squared_expected: real;
  decrypt_caesar_with_chi_squared_diff: real;
begin
  decrypt_caesar_with_chi_squared_alphabet_letters := cipher_alphabet;
  if Length(decrypt_caesar_with_chi_squared_alphabet_letters) = 0 then begin
  decrypt_caesar_with_chi_squared_alphabet_letters := default_alphabet();
end;
  decrypt_caesar_with_chi_squared_frequencies := frequencies_dict;
  if Length(decrypt_caesar_with_chi_squared_frequencies) = 0 then begin
  decrypt_caesar_with_chi_squared_frequencies := default_frequencies();
end;
  if not case_sensitive then begin
  ciphertext := LowerCase(ciphertext);
end;
  decrypt_caesar_with_chi_squared_best_shift := 0;
  decrypt_caesar_with_chi_squared_best_chi := 0;
  decrypt_caesar_with_chi_squared_best_text := '';
  decrypt_caesar_with_chi_squared_shift := 0;
  while decrypt_caesar_with_chi_squared_shift < Length(decrypt_caesar_with_chi_squared_alphabet_letters) do begin
  decrypt_caesar_with_chi_squared_decrypted := '';
  decrypt_caesar_with_chi_squared_i := 0;
  while decrypt_caesar_with_chi_squared_i < Length(ciphertext) do begin
  decrypt_caesar_with_chi_squared_ch := copy(ciphertext, decrypt_caesar_with_chi_squared_i+1, (decrypt_caesar_with_chi_squared_i + 1 - (decrypt_caesar_with_chi_squared_i)));
  decrypt_caesar_with_chi_squared_idx := index_of(decrypt_caesar_with_chi_squared_alphabet_letters, LowerCase(decrypt_caesar_with_chi_squared_ch));
  if decrypt_caesar_with_chi_squared_idx >= 0 then begin
  decrypt_caesar_with_chi_squared_m := Length(decrypt_caesar_with_chi_squared_alphabet_letters);
  decrypt_caesar_with_chi_squared_new_idx := (decrypt_caesar_with_chi_squared_idx - decrypt_caesar_with_chi_squared_shift) mod decrypt_caesar_with_chi_squared_m;
  if decrypt_caesar_with_chi_squared_new_idx < 0 then begin
  decrypt_caesar_with_chi_squared_new_idx := decrypt_caesar_with_chi_squared_new_idx + decrypt_caesar_with_chi_squared_m;
end;
  decrypt_caesar_with_chi_squared_new_char := decrypt_caesar_with_chi_squared_alphabet_letters[decrypt_caesar_with_chi_squared_new_idx];
  if case_sensitive and (decrypt_caesar_with_chi_squared_ch <> LowerCase(decrypt_caesar_with_chi_squared_ch)) then begin
  decrypt_caesar_with_chi_squared_decrypted := decrypt_caesar_with_chi_squared_decrypted + UpperCase(decrypt_caesar_with_chi_squared_new_char);
end else begin
  decrypt_caesar_with_chi_squared_decrypted := decrypt_caesar_with_chi_squared_decrypted + decrypt_caesar_with_chi_squared_new_char;
end;
end else begin
  decrypt_caesar_with_chi_squared_decrypted := decrypt_caesar_with_chi_squared_decrypted + decrypt_caesar_with_chi_squared_ch;
end;
  decrypt_caesar_with_chi_squared_i := decrypt_caesar_with_chi_squared_i + 1;
end;
  decrypt_caesar_with_chi_squared_chi := 0;
  decrypt_caesar_with_chi_squared_lowered := IfThen(case_sensitive, LowerCase(decrypt_caesar_with_chi_squared_decrypted), decrypt_caesar_with_chi_squared_decrypted);
  decrypt_caesar_with_chi_squared_j := 0;
  while decrypt_caesar_with_chi_squared_j < Length(decrypt_caesar_with_chi_squared_alphabet_letters) do begin
  decrypt_caesar_with_chi_squared_letter := decrypt_caesar_with_chi_squared_alphabet_letters[decrypt_caesar_with_chi_squared_j];
  decrypt_caesar_with_chi_squared_occ := count_char(decrypt_caesar_with_chi_squared_lowered, decrypt_caesar_with_chi_squared_letter);
  if decrypt_caesar_with_chi_squared_occ > 0 then begin
  decrypt_caesar_with_chi_squared_occf := Double(decrypt_caesar_with_chi_squared_occ);
  decrypt_caesar_with_chi_squared_expected := decrypt_caesar_with_chi_squared_frequencies[decrypt_caesar_with_chi_squared_letter] * decrypt_caesar_with_chi_squared_occf;
  decrypt_caesar_with_chi_squared_diff := decrypt_caesar_with_chi_squared_occf - decrypt_caesar_with_chi_squared_expected;
  decrypt_caesar_with_chi_squared_chi := decrypt_caesar_with_chi_squared_chi + (((decrypt_caesar_with_chi_squared_diff * decrypt_caesar_with_chi_squared_diff) / decrypt_caesar_with_chi_squared_expected) * decrypt_caesar_with_chi_squared_occf);
end;
  decrypt_caesar_with_chi_squared_j := decrypt_caesar_with_chi_squared_j + 1;
end;
  if (decrypt_caesar_with_chi_squared_shift = 0) or (decrypt_caesar_with_chi_squared_chi < decrypt_caesar_with_chi_squared_best_chi) then begin
  decrypt_caesar_with_chi_squared_best_shift := decrypt_caesar_with_chi_squared_shift;
  decrypt_caesar_with_chi_squared_best_chi := decrypt_caesar_with_chi_squared_chi;
  decrypt_caesar_with_chi_squared_best_text := decrypt_caesar_with_chi_squared_decrypted;
end;
  decrypt_caesar_with_chi_squared_shift := decrypt_caesar_with_chi_squared_shift + 1;
end;
  exit(makeResult(decrypt_caesar_with_chi_squared_best_shift, decrypt_caesar_with_chi_squared_best_chi, decrypt_caesar_with_chi_squared_best_text));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := decrypt_caesar_with_chi_squared('dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!', [], Map2(), false);
  writeln((((IntToStr(r1.shift) + ', ') + FloatToStr(r1.chi)) + ', ') + r1.decoded);
  r2 := decrypt_caesar_with_chi_squared('crybd cdbsxq', [], Map2(), false);
  writeln((((IntToStr(r2.shift) + ', ') + FloatToStr(r2.chi)) + ', ') + r2.decoded);
  r3 := decrypt_caesar_with_chi_squared('Crybd Cdbsxq', [], Map2(), true);
  writeln((((IntToStr(r3.shift) + ', ') + FloatToStr(r3.chi)) + ', ') + r3.decoded);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
