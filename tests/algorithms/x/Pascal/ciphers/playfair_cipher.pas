{$mode objfpc}
program Main;
uses SysUtils;
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
  key: string;
  cipher: string;
  xs: StrArray;
  plaintext: string;
  x: string;
  dirty: string;
function contains(xs: StrArray; x: string): boolean; forward;
function index_of(xs: StrArray; x: string): integer; forward;
function prepare_input(dirty: string): string; forward;
function generate_table(key: string): StrArray; forward;
function encode(plaintext: string; key: string): string; forward;
function decode(cipher: string; key: string): string; forward;
procedure main(); forward;
function contains(xs: StrArray; x: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = x then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function index_of(xs: StrArray; x: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(xs) do begin
  if xs[index_of_i] = x then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function prepare_input(dirty: string): string;
var
  prepare_input_letters: string;
  prepare_input_upper_dirty: string;
  prepare_input_filtered: string;
  prepare_input_i: integer;
  prepare_input_c: string;
  prepare_input_clean: string;
  prepare_input_c1: string;
  prepare_input_c2: string;
begin
  prepare_input_letters := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  prepare_input_upper_dirty := UpperCase(dirty);
  prepare_input_filtered := '';
  prepare_input_i := 0;
  while prepare_input_i < Length(prepare_input_upper_dirty) do begin
  prepare_input_c := copy(prepare_input_upper_dirty, prepare_input_i+1, (prepare_input_i + 1 - (prepare_input_i)));
  if Pos(prepare_input_c, prepare_input_letters) <> 0 then begin
  prepare_input_filtered := prepare_input_filtered + prepare_input_c;
end;
  prepare_input_i := prepare_input_i + 1;
end;
  if Length(prepare_input_filtered) < 2 then begin
  exit(prepare_input_filtered);
end;
  prepare_input_clean := '';
  prepare_input_i := 0;
  while prepare_input_i < (Length(prepare_input_filtered) - 1) do begin
  prepare_input_c1 := copy(prepare_input_filtered, prepare_input_i+1, (prepare_input_i + 1 - (prepare_input_i)));
  prepare_input_c2 := copy(prepare_input_filtered, prepare_input_i + 1+1, (prepare_input_i + 2 - (prepare_input_i + 1)));
  prepare_input_clean := prepare_input_clean + prepare_input_c1;
  if prepare_input_c1 = prepare_input_c2 then begin
  prepare_input_clean := prepare_input_clean + 'X';
end;
  prepare_input_i := prepare_input_i + 1;
end;
  prepare_input_clean := prepare_input_clean + copy(prepare_input_filtered, Length(prepare_input_filtered) - 1+1, (Length(prepare_input_filtered) - (Length(prepare_input_filtered) - 1)));
  if (Length(prepare_input_clean) mod 2) = 1 then begin
  prepare_input_clean := prepare_input_clean + 'X';
end;
  exit(prepare_input_clean);
end;
function generate_table(key: string): StrArray;
var
  generate_table_alphabet: string;
  generate_table_table: array of string;
  generate_table_upper_key: string;
  generate_table_i: integer;
  generate_table_c: string;
begin
  generate_table_alphabet := 'ABCDEFGHIKLMNOPQRSTUVWXYZ';
  generate_table_table := [];
  generate_table_upper_key := UpperCase(key);
  generate_table_i := 0;
  while generate_table_i < Length(generate_table_upper_key) do begin
  generate_table_c := copy(generate_table_upper_key, generate_table_i+1, (generate_table_i + 1 - (generate_table_i)));
  if Pos(generate_table_c, generate_table_alphabet) <> 0 then begin
  if not contains(generate_table_table, generate_table_c) then begin
  generate_table_table := concat(generate_table_table, StrArray([generate_table_c]));
end;
end;
  generate_table_i := generate_table_i + 1;
end;
  generate_table_i := 0;
  while generate_table_i < Length(generate_table_alphabet) do begin
  generate_table_c := copy(generate_table_alphabet, generate_table_i+1, (generate_table_i + 1 - (generate_table_i)));
  if not contains(generate_table_table, generate_table_c) then begin
  generate_table_table := concat(generate_table_table, StrArray([generate_table_c]));
end;
  generate_table_i := generate_table_i + 1;
end;
  exit(generate_table_table);
end;
function encode(plaintext: string; key: string): string;
var
  encode_table: StrArray;
  encode_text: string;
  encode_cipher: string;
  encode_i: integer;
  encode_c1: string;
  encode_c2: string;
  encode_idx1: integer;
  encode_idx2: integer;
  encode_row1: integer;
  encode_col1: integer;
  encode_row2: integer;
  encode_col2: integer;
begin
  encode_table := generate_table(key);
  encode_text := prepare_input(plaintext);
  encode_cipher := '';
  encode_i := 0;
  while encode_i < Length(encode_text) do begin
  encode_c1 := copy(encode_text, encode_i+1, (encode_i + 1 - (encode_i)));
  encode_c2 := copy(encode_text, encode_i + 1+1, (encode_i + 2 - (encode_i + 1)));
  encode_idx1 := index_of(encode_table, encode_c1);
  encode_idx2 := index_of(encode_table, encode_c2);
  encode_row1 := encode_idx1 div 5;
  encode_col1 := encode_idx1 mod 5;
  encode_row2 := encode_idx2 div 5;
  encode_col2 := encode_idx2 mod 5;
  if encode_row1 = encode_row2 then begin
  encode_cipher := encode_cipher + encode_table[(encode_row1 * 5) + ((encode_col1 + 1) mod 5)];
  encode_cipher := encode_cipher + encode_table[(encode_row2 * 5) + ((encode_col2 + 1) mod 5)];
end else begin
  if encode_col1 = encode_col2 then begin
  encode_cipher := encode_cipher + encode_table[(((encode_row1 + 1) mod 5) * 5) + encode_col1];
  encode_cipher := encode_cipher + encode_table[(((encode_row2 + 1) mod 5) * 5) + encode_col2];
end else begin
  encode_cipher := encode_cipher + encode_table[(encode_row1 * 5) + encode_col2];
  encode_cipher := encode_cipher + encode_table[(encode_row2 * 5) + encode_col1];
end;
end;
  encode_i := encode_i + 2;
end;
  exit(encode_cipher);
end;
function decode(cipher: string; key: string): string;
var
  decode_table: StrArray;
  decode_plain: string;
  decode_i: integer;
  decode_c1: string;
  decode_c2: string;
  decode_idx1: integer;
  decode_idx2: integer;
  decode_row1: integer;
  decode_col1: integer;
  decode_row2: integer;
  decode_col2: integer;
begin
  decode_table := generate_table(key);
  decode_plain := '';
  decode_i := 0;
  while decode_i < Length(cipher) do begin
  decode_c1 := copy(cipher, decode_i+1, (decode_i + 1 - (decode_i)));
  decode_c2 := copy(cipher, decode_i + 1+1, (decode_i + 2 - (decode_i + 1)));
  decode_idx1 := index_of(decode_table, decode_c1);
  decode_idx2 := index_of(decode_table, decode_c2);
  decode_row1 := decode_idx1 div 5;
  decode_col1 := decode_idx1 mod 5;
  decode_row2 := decode_idx2 div 5;
  decode_col2 := decode_idx2 mod 5;
  if decode_row1 = decode_row2 then begin
  decode_plain := decode_plain + decode_table[(decode_row1 * 5) + ((decode_col1 + 4) mod 5)];
  decode_plain := decode_plain + decode_table[(decode_row2 * 5) + ((decode_col2 + 4) mod 5)];
end else begin
  if decode_col1 = decode_col2 then begin
  decode_plain := decode_plain + decode_table[(((decode_row1 + 4) mod 5) * 5) + decode_col1];
  decode_plain := decode_plain + decode_table[(((decode_row2 + 4) mod 5) * 5) + decode_col2];
end else begin
  decode_plain := decode_plain + decode_table[(decode_row1 * 5) + decode_col2];
  decode_plain := decode_plain + decode_table[(decode_row2 * 5) + decode_col1];
end;
end;
  decode_i := decode_i + 2;
end;
  exit(decode_plain);
end;
procedure main();
begin
  writeln('Encoded:', ' ', encode('BYE AND THANKS', 'GREETING'));
  writeln('Decoded:', ' ', decode('CXRBANRLBALQ', 'GREETING'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
