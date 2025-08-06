{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  abc: string;
  low_abc: string;
  rotor1: string;
  rotor2: string;
  rotor3: string;
  rotor4: string;
  rotor5: string;
  rotor6: string;
  rotor7: string;
  rotor8: string;
  rotor9: string;
  reflector_pairs: array of string;
  pb: string;
  pbstring: string;
  rotor_selection: StrArray;
  ch: string;
  s: string;
  text: string;
  rotpos: IntArray;
  plugb: string;
  x: string;
  xs: StrArray;
  rotsel: StrArray;
  rotor_position: IntArray;
function list_contains(xs: StrArray; x: string): boolean; forward;
function index_in_string(s: string; ch: string): integer; forward;
function contains_char(s: string; ch: string): boolean; forward;
function to_uppercase(s: string): string; forward;
function plugboard_map(pb: StrArray; ch: string): string; forward;
function reflector_map(ch: string): string; forward;
function count_unique(xs: StrArray): integer; forward;
function build_plugboard(pbstring: string): StrArray; forward;
procedure validator(rotpos: IntArray; rotsel: StrArray; pb: string); forward;
function enigma(text: string; rotor_position: IntArray; rotor_selection: StrArray; plugb: string): string; forward;
procedure main(); forward;
function list_contains(xs: StrArray; x: string): boolean;
var
  list_contains_i: integer;
begin
  list_contains_i := 0;
  while list_contains_i < Length(xs) do begin
  if xs[list_contains_i] = x then begin
  exit(true);
end;
  list_contains_i := list_contains_i + 1;
end;
  exit(false);
end;
function index_in_string(s: string; ch: string): integer;
var
  index_in_string_i: integer;
begin
  index_in_string_i := 0;
  while index_in_string_i < Length(s) do begin
  if copy(s, index_in_string_i+1, (index_in_string_i + 1 - (index_in_string_i))) = ch then begin
  exit(index_in_string_i);
end;
  index_in_string_i := index_in_string_i + 1;
end;
  exit(-1);
end;
function contains_char(s: string; ch: string): boolean;
begin
  exit(index_in_string(s, ch) >= 0);
end;
function to_uppercase(s: string): string;
var
  to_uppercase_res: string;
  to_uppercase_i: integer;
  to_uppercase_ch: string;
  to_uppercase_idx: integer;
begin
  to_uppercase_res := '';
  to_uppercase_i := 0;
  while to_uppercase_i < Length(s) do begin
  to_uppercase_ch := copy(s, to_uppercase_i+1, (to_uppercase_i + 1 - (to_uppercase_i)));
  to_uppercase_idx := index_in_string(low_abc, to_uppercase_ch);
  if to_uppercase_idx >= 0 then begin
  to_uppercase_res := to_uppercase_res + copy(abc, to_uppercase_idx+1, (to_uppercase_idx + 1 - (to_uppercase_idx)));
end else begin
  to_uppercase_res := to_uppercase_res + to_uppercase_ch;
end;
  to_uppercase_i := to_uppercase_i + 1;
end;
  exit(to_uppercase_res);
end;
function plugboard_map(pb: StrArray; ch: string): string;
var
  plugboard_map_i: integer;
  plugboard_map_pair: string;
  plugboard_map_a: string;
  plugboard_map_b: string;
begin
  plugboard_map_i := 0;
  while plugboard_map_i < Length(pb) do begin
  plugboard_map_pair := pb[plugboard_map_i];
  plugboard_map_a := copy(plugboard_map_pair, 0+1, (1 - (0)));
  plugboard_map_b := copy(plugboard_map_pair, 1+1, (2 - (1)));
  if ch = plugboard_map_a then begin
  exit(plugboard_map_b);
end;
  if ch = plugboard_map_b then begin
  exit(plugboard_map_a);
end;
  plugboard_map_i := plugboard_map_i + 1;
end;
  exit(ch);
end;
function reflector_map(ch: string): string;
var
  reflector_map_i: integer;
  reflector_map_pair: string;
  reflector_map_a: string;
  reflector_map_b: string;
begin
  reflector_map_i := 0;
  while reflector_map_i < Length(reflector_pairs) do begin
  reflector_map_pair := reflector_pairs[reflector_map_i];
  reflector_map_a := copy(reflector_map_pair, 0+1, (1 - (0)));
  reflector_map_b := copy(reflector_map_pair, 1+1, (2 - (1)));
  if ch = reflector_map_a then begin
  exit(reflector_map_b);
end;
  if ch = reflector_map_b then begin
  exit(reflector_map_a);
end;
  reflector_map_i := reflector_map_i + 1;
end;
  exit(ch);
end;
function count_unique(xs: StrArray): integer;
var
  count_unique_unique: array of string;
  count_unique_i: integer;
begin
  count_unique_unique := [];
  count_unique_i := 0;
  while count_unique_i < Length(xs) do begin
  if not list_contains(count_unique_unique, xs[count_unique_i]) then begin
  count_unique_unique := concat(count_unique_unique, StrArray([xs[count_unique_i]]));
end;
  count_unique_i := count_unique_i + 1;
end;
  exit(Length(count_unique_unique));
end;
function build_plugboard(pbstring: string): StrArray;
var
  build_plugboard_pbstring_nospace: string;
  build_plugboard_i: integer;
  build_plugboard_ch: string;
  build_plugboard_seen: array of string;
  build_plugboard_pb: array of string;
  build_plugboard_a: string;
  build_plugboard_b: string;
begin
  if Length(pbstring) = 0 then begin
  exit(StrArray([]));
end;
  if (Length(pbstring) mod 2) <> 0 then begin
  panic(('Odd number of symbols(' + IntToStr(Length(pbstring))) + ')');
end;
  build_plugboard_pbstring_nospace := '';
  build_plugboard_i := 0;
  while build_plugboard_i < Length(pbstring) do begin
  build_plugboard_ch := copy(pbstring, build_plugboard_i+1, (build_plugboard_i + 1 - (build_plugboard_i)));
  if build_plugboard_ch <> ' ' then begin
  build_plugboard_pbstring_nospace := build_plugboard_pbstring_nospace + build_plugboard_ch;
end;
  build_plugboard_i := build_plugboard_i + 1;
end;
  build_plugboard_seen := [];
  build_plugboard_i := 0;
  while build_plugboard_i < Length(build_plugboard_pbstring_nospace) do begin
  build_plugboard_ch := copy(build_plugboard_pbstring_nospace, build_plugboard_i+1, (build_plugboard_i + 1 - (build_plugboard_i)));
  if not contains_char(abc, build_plugboard_ch) then begin
  panic(('''' + build_plugboard_ch) + ''' not in list of symbols');
end;
  if list_contains(build_plugboard_seen, build_plugboard_ch) then begin
  panic(('Duplicate symbol(' + build_plugboard_ch) + ')');
end;
  build_plugboard_seen := concat(build_plugboard_seen, StrArray([build_plugboard_ch]));
  build_plugboard_i := build_plugboard_i + 1;
end;
  build_plugboard_pb := [];
  build_plugboard_i := 0;
  while build_plugboard_i < (Length(build_plugboard_pbstring_nospace) - 1) do begin
  build_plugboard_a := copy(build_plugboard_pbstring_nospace, build_plugboard_i+1, (build_plugboard_i + 1 - (build_plugboard_i)));
  build_plugboard_b := copy(build_plugboard_pbstring_nospace, build_plugboard_i + 1+1, (build_plugboard_i + 2 - (build_plugboard_i + 1)));
  build_plugboard_pb := concat(build_plugboard_pb, StrArray([build_plugboard_a + build_plugboard_b]));
  build_plugboard_i := build_plugboard_i + 2;
end;
  exit(build_plugboard_pb);
end;
procedure validator(rotpos: IntArray; rotsel: StrArray; pb: string);
var
  validator_r1: integer;
  validator_r2: integer;
  validator_r3: integer;
begin
  if count_unique(rotsel) < 3 then begin
  panic(('Please use 3 unique rotors (not ' + IntToStr(count_unique(rotsel))) + ')');
end;
  if Length(rotpos) <> 3 then begin
  panic('Rotor position must have 3 values');
end;
  validator_r1 := rotpos[0];
  validator_r2 := rotpos[1];
  validator_r3 := rotpos[2];
  if not ((0 < validator_r1) and (validator_r1 <= Length(abc))) then begin
  panic(('First rotor position is not within range of 1..26 (' + IntToStr(validator_r1)) + ')');
end;
  if not ((0 < validator_r2) and (validator_r2 <= Length(abc))) then begin
  panic(('Second rotor position is not within range of 1..26 (' + IntToStr(validator_r2)) + ')');
end;
  if not ((0 < validator_r3) and (validator_r3 <= Length(abc))) then begin
  panic(('Third rotor position is not within range of 1..26 (' + IntToStr(validator_r3)) + ')');
end;
end;
function enigma(text: string; rotor_position: IntArray; rotor_selection: StrArray; plugb: string): string;
var
  enigma_up_text: string;
  enigma_up_pb: string;
  enigma_plugboard: StrArray;
  enigma_rotorpos1: integer;
  enigma_rotorpos2: integer;
  enigma_rotorpos3: integer;
  enigma_rotor_a: string;
  enigma_rotor_b: string;
  enigma_rotor_c: string;
  enigma_result_: string;
  enigma_i: integer;
  enigma_symbol: string;
  enigma_index: integer;
begin
  enigma_up_text := to_uppercase(text);
  enigma_up_pb := to_uppercase(plugb);
  validator(rotor_position, rotor_selection, enigma_up_pb);
  enigma_plugboard := build_plugboard(enigma_up_pb);
  enigma_rotorpos1 := rotor_position[0] - 1;
  enigma_rotorpos2 := rotor_position[1] - 1;
  enigma_rotorpos3 := rotor_position[2] - 1;
  enigma_rotor_a := rotor_selection[0];
  enigma_rotor_b := rotor_selection[1];
  enigma_rotor_c := rotor_selection[2];
  enigma_result_ := '';
  enigma_i := 0;
  while enigma_i < Length(enigma_up_text) do begin
  enigma_symbol := copy(enigma_up_text, enigma_i+1, (enigma_i + 1 - (enigma_i)));
  if contains_char(abc, enigma_symbol) then begin
  enigma_symbol := plugboard_map(enigma_plugboard, enigma_symbol);
  enigma_index := index_in_string(abc, enigma_symbol) + enigma_rotorpos1;
  enigma_symbol := copy(enigma_rotor_a, enigma_index mod Length(abc)+1, ((enigma_index mod Length(abc)) + 1 - (enigma_index mod Length(abc))));
  enigma_index := index_in_string(abc, enigma_symbol) + enigma_rotorpos2;
  enigma_symbol := copy(enigma_rotor_b, enigma_index mod Length(abc)+1, ((enigma_index mod Length(abc)) + 1 - (enigma_index mod Length(abc))));
  enigma_index := index_in_string(abc, enigma_symbol) + enigma_rotorpos3;
  enigma_symbol := copy(enigma_rotor_c, enigma_index mod Length(abc)+1, ((enigma_index mod Length(abc)) + 1 - (enigma_index mod Length(abc))));
  enigma_symbol := reflector_map(enigma_symbol);
  enigma_index := index_in_string(enigma_rotor_c, enigma_symbol) - enigma_rotorpos3;
  if enigma_index < 0 then begin
  enigma_index := enigma_index + Length(abc);
end;
  enigma_symbol := copy(abc, enigma_index+1, (enigma_index + 1 - (enigma_index)));
  enigma_index := index_in_string(enigma_rotor_b, enigma_symbol) - enigma_rotorpos2;
  if enigma_index < 0 then begin
  enigma_index := enigma_index + Length(abc);
end;
  enigma_symbol := copy(abc, enigma_index+1, (enigma_index + 1 - (enigma_index)));
  enigma_index := index_in_string(enigma_rotor_a, enigma_symbol) - enigma_rotorpos1;
  if enigma_index < 0 then begin
  enigma_index := enigma_index + Length(abc);
end;
  enigma_symbol := copy(abc, enigma_index+1, (enigma_index + 1 - (enigma_index)));
  enigma_symbol := plugboard_map(enigma_plugboard, enigma_symbol);
  enigma_rotorpos1 := enigma_rotorpos1 + 1;
  if enigma_rotorpos1 >= Length(abc) then begin
  enigma_rotorpos1 := 0;
  enigma_rotorpos2 := enigma_rotorpos2 + 1;
end;
  if enigma_rotorpos2 >= Length(abc) then begin
  enigma_rotorpos2 := 0;
  enigma_rotorpos3 := enigma_rotorpos3 + 1;
end;
  if enigma_rotorpos3 >= Length(abc) then begin
  enigma_rotorpos3 := 0;
end;
end;
  enigma_result_ := enigma_result_ + enigma_symbol;
  enigma_i := enigma_i + 1;
end;
  exit(enigma_result_);
end;
procedure main();
var
  main_message: string;
  main_rotor_pos: array of integer;
  main_pb: string;
  main_rotor_sel: array of string;
  main_en: string;
begin
  main_message := 'This is my Python script that emulates the Enigma machine from WWII.';
  main_rotor_pos := [1, 1, 1];
  main_pb := 'pictures';
  main_rotor_sel := [rotor2, rotor4, rotor8];
  main_en := enigma(main_message, main_rotor_pos, main_rotor_sel, main_pb);
  writeln('Encrypted message: ' + main_en);
  writeln('Decrypted message: ' + enigma(main_en, main_rotor_pos, main_rotor_sel, main_pb));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  abc := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  low_abc := 'abcdefghijklmnopqrstuvwxyz';
  rotor1 := 'EGZWVONAHDCLFQMSIPJBYUKXTR';
  rotor2 := 'FOBHMDKEXQNRAULPGSJVTYICZW';
  rotor3 := 'ZJXESIUQLHAVRMDOYGTNFWPBKC';
  rotor4 := 'RMDJXFUWGISLHVTCQNKYPBEZOA';
  rotor5 := 'SGLCPQWZHKXAREONTFBVIYJUDM';
  rotor6 := 'HVSICLTYKQUBXDWAJZOMFGPREN';
  rotor7 := 'RZWQHFMVDBKICJLNTUXAGYPSOE';
  rotor8 := 'LFKIJODBEGAMQPXVUHYSTCZRWN';
  rotor9 := 'KOAEGVDHXPQZMLFTYWJNBRCIUS';
  reflector_pairs := ['AN', 'BO', 'CP', 'DQ', 'ER', 'FS', 'GT', 'HU', 'IV', 'JW', 'KX', 'LY', 'MZ'];
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
