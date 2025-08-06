{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  encode_map: specialize TFPGMap<string, string>;
  make_decode_map_k_idx: integer;
  decode_map: specialize TFPGMap<string, string>;
  word: string;
  coded: string;
  s: string;
function Map1(): specialize TFPGMap<string, string>; forward;
function make_decode_map(): specialize TFPGMap<string, string>; forward;
function split_spaces(s: string): StrArray; forward;
function encode(word: string): string; forward;
function decode(coded: string): string; forward;
function Map1(): specialize TFPGMap<string, string>;
begin
  Result := specialize TFPGMap<string, string>.Create();
  Result.AddOrSetData('a', Variant('AAAAA'));
  Result.AddOrSetData('b', Variant('AAAAB'));
  Result.AddOrSetData('c', Variant('AAABA'));
  Result.AddOrSetData('d', Variant('AAABB'));
  Result.AddOrSetData('e', Variant('AABAA'));
  Result.AddOrSetData('f', Variant('AABAB'));
  Result.AddOrSetData('g', Variant('AABBA'));
  Result.AddOrSetData('h', Variant('AABBB'));
  Result.AddOrSetData('i', Variant('ABAAA'));
  Result.AddOrSetData('j', Variant('BBBAA'));
  Result.AddOrSetData('k', Variant('ABAAB'));
  Result.AddOrSetData('l', Variant('ABABA'));
  Result.AddOrSetData('m', Variant('ABABB'));
  Result.AddOrSetData('n', Variant('ABBAA'));
  Result.AddOrSetData('o', Variant('ABBAB'));
  Result.AddOrSetData('p', Variant('ABBBA'));
  Result.AddOrSetData('q', Variant('ABBBB'));
  Result.AddOrSetData('r', Variant('BAAAA'));
  Result.AddOrSetData('s', Variant('BAAAB'));
  Result.AddOrSetData('t', Variant('BAABA'));
  Result.AddOrSetData('u', Variant('BAABB'));
  Result.AddOrSetData('v', Variant('BBBAB'));
  Result.AddOrSetData('w', Variant('BABAA'));
  Result.AddOrSetData('x', Variant('BABAB'));
  Result.AddOrSetData('y', Variant('BABBA'));
  Result.AddOrSetData('z', Variant('BABBB'));
  Result.AddOrSetData(' ', Variant(' '));
end;
function make_decode_map(): specialize TFPGMap<string, string>;
var
  make_decode_map_m: specialize TFPGMap<string, string>;
  make_decode_map_k: string;
begin
  make_decode_map_m := specialize TFPGMap<string, string>.Create();
  for make_decode_map_k_idx := 0 to (encode_map.Count - 1) do begin
  make_decode_map_k := encode_map.Keys[make_decode_map_k_idx];
  make_decode_map_m.AddOrSetData(encode_map[make_decode_map_k], Variant(make_decode_map_k));
end;
  exit(make_decode_map_m);
end;
function split_spaces(s: string): StrArray;
var
  split_spaces_parts: array of string;
  split_spaces_current: string;
  split_spaces_i: integer;
  split_spaces_ch: string;
begin
  split_spaces_parts := [];
  split_spaces_current := '';
  split_spaces_i := 0;
  while split_spaces_i < Length(s) do begin
  split_spaces_ch := copy(s, split_spaces_i+1, (split_spaces_i + 1 - (split_spaces_i)));
  if split_spaces_ch = ' ' then begin
  split_spaces_parts := concat(split_spaces_parts, StrArray([split_spaces_current]));
  split_spaces_current := '';
end else begin
  split_spaces_current := split_spaces_current + split_spaces_ch;
end;
  split_spaces_i := split_spaces_i + 1;
end;
  split_spaces_parts := concat(split_spaces_parts, StrArray([split_spaces_current]));
  exit(split_spaces_parts);
end;
function encode(word: string): string;
var
  encode_w: string;
  encode_encoded: string;
  encode_i: integer;
  encode_ch: string;
begin
  encode_w := LowerCase(word);
  encode_encoded := '';
  encode_i := 0;
  while encode_i < Length(encode_w) do begin
  encode_ch := copy(encode_w, encode_i+1, (encode_i + 1 - (encode_i)));
  if encode_map.IndexOf(encode_ch) <> -1 then begin
  encode_encoded := encode_encoded + encode_map[encode_ch];
end else begin
  panic('encode() accepts only letters of the alphabet and spaces');
end;
  encode_i := encode_i + 1;
end;
  exit(encode_encoded);
end;
function decode(coded: string): string;
var
  decode_i: integer;
  decode_ch: string;
  decode_words: StrArray;
  decode_decoded: string;
  decode_w: integer;
  decode_word: string;
  decode_j: integer;
  decode_segment: string;
begin
  decode_i := 0;
  while decode_i < Length(coded) do begin
  decode_ch := copy(coded, decode_i+1, (decode_i + 1 - (decode_i)));
  if ((decode_ch <> 'A') and (decode_ch <> 'B')) and (decode_ch <> ' ') then begin
  panic('decode() accepts only ''A'', ''B'' and spaces');
end;
  decode_i := decode_i + 1;
end;
  decode_words := split_spaces(coded);
  decode_decoded := '';
  decode_w := 0;
  while decode_w < Length(decode_words) do begin
  decode_word := decode_words[decode_w];
  decode_j := 0;
  while decode_j < Length(decode_word) do begin
  decode_segment := copy(decode_word, decode_j+1, (decode_j + 5 - (decode_j)));
  decode_decoded := decode_decoded + decode_map[decode_segment];
  decode_j := decode_j + 5;
end;
  if decode_w < (Length(decode_words) - 1) then begin
  decode_decoded := decode_decoded + ' ';
end;
  decode_w := decode_w + 1;
end;
  exit(decode_decoded);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  encode_map := Map1();
  decode_map := make_decode_map();
  writeln(encode('hello'));
  writeln(encode('hello world'));
  writeln(decode('AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB'));
  writeln(decode('AABBBAABAAABABAABABAABBAB'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
