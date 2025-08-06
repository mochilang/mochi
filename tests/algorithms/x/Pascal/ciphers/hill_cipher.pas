{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
  KEY_STRING: string;
  key: array of array of integer;
  n: integer;
  v: IntArray;
  c: string;
  letter: string;
  break_key: integer;
  m: IntArrayArray;
  col: integer;
  a: integer;
  num: integer;
  text: string;
  s: integer;
  b: integer;
  row: integer;
function mod36(n: integer): integer; forward;
function gcd(a: integer; b: integer): integer; forward;
function replace_letters(letter: string): integer; forward;
function replace_digits(num: integer): string; forward;
function to_upper(c: string): string; forward;
function process_text(text: string; break_key: integer): string; forward;
function matrix_minor(m: IntArrayArray; row: integer; col: integer): IntArrayArray; forward;
function determinant(m: IntArrayArray): integer; forward;
function cofactor_matrix(m: IntArrayArray): IntArrayArray; forward;
function transpose(m: IntArrayArray): IntArrayArray; forward;
function matrix_mod(m: IntArrayArray): IntArrayArray; forward;
function scalar_matrix_mult(s: integer; m: IntArrayArray): IntArrayArray; forward;
function adjugate(m: IntArrayArray): IntArrayArray; forward;
function multiply_matrix_vector(m: IntArrayArray; v: IntArray): IntArray; forward;
function inverse_key(key: IntArrayArray): IntArrayArray; forward;
function hill_encrypt(key: IntArrayArray; text: string): string; forward;
function hill_decrypt(key: IntArrayArray; text: string): string; forward;
function mod36(n: integer): integer;
var
  mod36_r: integer;
begin
  mod36_r := n mod 36;
  if mod36_r < 0 then begin
  mod36_r := mod36_r + 36;
end;
  exit(mod36_r);
end;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_t: integer;
begin
  gcd_x := a;
  gcd_y := b;
  while gcd_y <> 0 do begin
  gcd_t := gcd_y;
  gcd_y := gcd_x mod gcd_y;
  gcd_x := gcd_t;
end;
  if gcd_x < 0 then begin
  gcd_x := -gcd_x;
end;
  exit(gcd_x);
end;
function replace_letters(letter: string): integer;
var
  replace_letters_i: integer;
begin
  replace_letters_i := 0;
  while replace_letters_i < Length(KEY_STRING) do begin
  if KEY_STRING[replace_letters_i+1] = letter then begin
  exit(replace_letters_i);
end;
  replace_letters_i := replace_letters_i + 1;
end;
  exit(0);
end;
function replace_digits(num: integer): string;
var
  replace_digits_idx: integer;
begin
  replace_digits_idx := mod36(num);
  exit(KEY_STRING[replace_digits_idx+1]);
end;
function to_upper(c: string): string;
var
  to_upper_lower: string;
  to_upper_upper: string;
  to_upper_i: integer;
begin
  to_upper_lower := 'abcdefghijklmnopqrstuvwxyz';
  to_upper_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  to_upper_i := 0;
  while to_upper_i < Length(to_upper_lower) do begin
  if c = to_upper_lower[to_upper_i+1] then begin
  exit(to_upper_upper[to_upper_i+1]);
end;
  to_upper_i := to_upper_i + 1;
end;
  exit(c);
end;
function process_text(text: string; break_key: integer): string;
var
  process_text_chars: array of string;
  process_text_i: integer;
  process_text_c: string;
  process_text_j: integer;
  process_text_ok: boolean;
  process_text_last: string;
  process_text_res: string;
  process_text_k: integer;
begin
  process_text_chars := [];
  process_text_i := 0;
  while process_text_i < Length(text) do begin
  process_text_c := to_upper(text[process_text_i+1]);
  process_text_j := 0;
  process_text_ok := false;
  while process_text_j < Length(KEY_STRING) do begin
  if KEY_STRING[process_text_j+1] = process_text_c then begin
  process_text_ok := true;
  break;
end;
  process_text_j := process_text_j + 1;
end;
  if process_text_ok then begin
  process_text_chars := concat(process_text_chars, StrArray([process_text_c]));
end;
  process_text_i := process_text_i + 1;
end;
  if Length(process_text_chars) = 0 then begin
  exit('');
end;
  process_text_last := process_text_chars[Length(process_text_chars) - 1];
  while (Length(process_text_chars) mod break_key) <> 0 do begin
  process_text_chars := concat(process_text_chars, StrArray([process_text_last]));
end;
  process_text_res := '';
  process_text_k := 0;
  while process_text_k < Length(process_text_chars) do begin
  process_text_res := process_text_res + process_text_chars[process_text_k];
  process_text_k := process_text_k + 1;
end;
  exit(process_text_res);
end;
function matrix_minor(m: IntArrayArray; row: integer; col: integer): IntArrayArray;
var
  matrix_minor_res: array of IntArray;
  matrix_minor_i: integer;
  matrix_minor_r: array of integer;
  matrix_minor_j: integer;
begin
  matrix_minor_res := [];
  matrix_minor_i := 0;
  while matrix_minor_i < Length(m) do begin
  if matrix_minor_i <> row then begin
  matrix_minor_r := [];
  matrix_minor_j := 0;
  while matrix_minor_j < Length(m[matrix_minor_i]) do begin
  if matrix_minor_j <> col then begin
  matrix_minor_r := concat(matrix_minor_r, IntArray([m[matrix_minor_i][matrix_minor_j]]));
end;
  matrix_minor_j := matrix_minor_j + 1;
end;
  matrix_minor_res := concat(matrix_minor_res, [matrix_minor_r]);
end;
  matrix_minor_i := matrix_minor_i + 1;
end;
  exit(matrix_minor_res);
end;
function determinant(m: IntArrayArray): integer;
var
  determinant_n: integer;
  determinant_det: integer;
  determinant_col: integer;
  determinant_minor_mat: IntArrayArray;
  determinant_sign: integer;
begin
  determinant_n := Length(m);
  if determinant_n = 1 then begin
  exit(m[0][0]);
end;
  if determinant_n = 2 then begin
  exit((m[0][0] * m[1][1]) - (m[0][1] * m[1][0]));
end;
  determinant_det := 0;
  determinant_col := 0;
  while determinant_col < determinant_n do begin
  determinant_minor_mat := matrix_minor(m, 0, determinant_col);
  determinant_sign := 1;
  if (determinant_col mod 2) = 1 then begin
  determinant_sign := -1;
end;
  determinant_det := determinant_det + ((determinant_sign * m[0][determinant_col]) * determinant(determinant_minor_mat));
  determinant_col := determinant_col + 1;
end;
  exit(determinant_det);
end;
function cofactor_matrix(m: IntArrayArray): IntArrayArray;
var
  cofactor_matrix_n: integer;
  cofactor_matrix_res: array of IntArray;
  cofactor_matrix_i: integer;
  cofactor_matrix_row: array of integer;
  cofactor_matrix_j: integer;
  cofactor_matrix_minor_mat: IntArrayArray;
  cofactor_matrix_det_minor: integer;
  cofactor_matrix_sign: integer;
begin
  cofactor_matrix_n := Length(m);
  cofactor_matrix_res := [];
  cofactor_matrix_i := 0;
  while cofactor_matrix_i < cofactor_matrix_n do begin
  cofactor_matrix_row := [];
  cofactor_matrix_j := 0;
  while cofactor_matrix_j < cofactor_matrix_n do begin
  cofactor_matrix_minor_mat := matrix_minor(m, cofactor_matrix_i, cofactor_matrix_j);
  cofactor_matrix_det_minor := determinant(cofactor_matrix_minor_mat);
  cofactor_matrix_sign := 1;
  if ((cofactor_matrix_i + cofactor_matrix_j) mod 2) = 1 then begin
  cofactor_matrix_sign := -1;
end;
  cofactor_matrix_row := concat(cofactor_matrix_row, IntArray([cofactor_matrix_sign * cofactor_matrix_det_minor]));
  cofactor_matrix_j := cofactor_matrix_j + 1;
end;
  cofactor_matrix_res := concat(cofactor_matrix_res, [cofactor_matrix_row]);
  cofactor_matrix_i := cofactor_matrix_i + 1;
end;
  exit(cofactor_matrix_res);
end;
function transpose(m: IntArrayArray): IntArrayArray;
var
  transpose_rows: integer;
  transpose_cols: integer;
  transpose_res: array of IntArray;
  transpose_j: integer;
  transpose_row: array of integer;
  transpose_i: integer;
begin
  transpose_rows := Length(m);
  transpose_cols := Length(m[0]);
  transpose_res := [];
  transpose_j := 0;
  while transpose_j < transpose_cols do begin
  transpose_row := [];
  transpose_i := 0;
  while transpose_i < transpose_rows do begin
  transpose_row := concat(transpose_row, IntArray([m[transpose_i][transpose_j]]));
  transpose_i := transpose_i + 1;
end;
  transpose_res := concat(transpose_res, [transpose_row]);
  transpose_j := transpose_j + 1;
end;
  exit(transpose_res);
end;
function matrix_mod(m: IntArrayArray): IntArrayArray;
var
  matrix_mod_res: array of IntArray;
  matrix_mod_i: integer;
  matrix_mod_row: array of integer;
  matrix_mod_j: integer;
begin
  matrix_mod_res := [];
  matrix_mod_i := 0;
  while matrix_mod_i < Length(m) do begin
  matrix_mod_row := [];
  matrix_mod_j := 0;
  while matrix_mod_j < Length(m[matrix_mod_i]) do begin
  matrix_mod_row := concat(matrix_mod_row, IntArray([mod36(m[matrix_mod_i][matrix_mod_j])]));
  matrix_mod_j := matrix_mod_j + 1;
end;
  matrix_mod_res := concat(matrix_mod_res, [matrix_mod_row]);
  matrix_mod_i := matrix_mod_i + 1;
end;
  exit(matrix_mod_res);
end;
function scalar_matrix_mult(s: integer; m: IntArrayArray): IntArrayArray;
var
  scalar_matrix_mult_res: array of IntArray;
  scalar_matrix_mult_i: integer;
  scalar_matrix_mult_row: array of integer;
  scalar_matrix_mult_j: integer;
begin
  scalar_matrix_mult_res := [];
  scalar_matrix_mult_i := 0;
  while scalar_matrix_mult_i < Length(m) do begin
  scalar_matrix_mult_row := [];
  scalar_matrix_mult_j := 0;
  while scalar_matrix_mult_j < Length(m[scalar_matrix_mult_i]) do begin
  scalar_matrix_mult_row := concat(scalar_matrix_mult_row, IntArray([mod36(s * m[scalar_matrix_mult_i][scalar_matrix_mult_j])]));
  scalar_matrix_mult_j := scalar_matrix_mult_j + 1;
end;
  scalar_matrix_mult_res := concat(scalar_matrix_mult_res, [scalar_matrix_mult_row]);
  scalar_matrix_mult_i := scalar_matrix_mult_i + 1;
end;
  exit(scalar_matrix_mult_res);
end;
function adjugate(m: IntArrayArray): IntArrayArray;
var
  adjugate_cof: IntArrayArray;
  adjugate_n: integer;
  adjugate_res: array of IntArray;
  adjugate_i: integer;
  adjugate_row: array of integer;
  adjugate_j: integer;
begin
  adjugate_cof := cofactor_matrix(m);
  adjugate_n := Length(adjugate_cof);
  adjugate_res := [];
  adjugate_i := 0;
  while adjugate_i < adjugate_n do begin
  adjugate_row := [];
  adjugate_j := 0;
  while adjugate_j < adjugate_n do begin
  adjugate_row := concat(adjugate_row, IntArray([adjugate_cof[adjugate_j][adjugate_i]]));
  adjugate_j := adjugate_j + 1;
end;
  adjugate_res := concat(adjugate_res, [adjugate_row]);
  adjugate_i := adjugate_i + 1;
end;
  exit(adjugate_res);
end;
function multiply_matrix_vector(m: IntArrayArray; v: IntArray): IntArray;
var
  multiply_matrix_vector_n: integer;
  multiply_matrix_vector_res: array of integer;
  multiply_matrix_vector_i: integer;
  multiply_matrix_vector_sum: integer;
  multiply_matrix_vector_j: integer;
begin
  multiply_matrix_vector_n := Length(m);
  multiply_matrix_vector_res := [];
  multiply_matrix_vector_i := 0;
  while multiply_matrix_vector_i < multiply_matrix_vector_n do begin
  multiply_matrix_vector_sum := 0;
  multiply_matrix_vector_j := 0;
  while multiply_matrix_vector_j < multiply_matrix_vector_n do begin
  multiply_matrix_vector_sum := multiply_matrix_vector_sum + (m[multiply_matrix_vector_i][multiply_matrix_vector_j] * v[multiply_matrix_vector_j]);
  multiply_matrix_vector_j := multiply_matrix_vector_j + 1;
end;
  multiply_matrix_vector_res := concat(multiply_matrix_vector_res, IntArray([mod36(multiply_matrix_vector_sum)]));
  multiply_matrix_vector_i := multiply_matrix_vector_i + 1;
end;
  exit(multiply_matrix_vector_res);
end;
function inverse_key(key: IntArrayArray): IntArrayArray;
var
  inverse_key_det_val: integer;
  inverse_key_det_mod: integer;
  inverse_key_det_inv: integer;
  inverse_key_i: integer;
  inverse_key_adj: IntArrayArray;
  inverse_key_tmp: IntArrayArray;
  inverse_key_res: IntArrayArray;
begin
  inverse_key_det_val := determinant(key);
  inverse_key_det_mod := mod36(inverse_key_det_val);
  inverse_key_det_inv := 0;
  inverse_key_i := 0;
  while inverse_key_i < 36 do begin
  if ((inverse_key_det_mod * inverse_key_i) mod 36) = 1 then begin
  inverse_key_det_inv := inverse_key_i;
  break;
end;
  inverse_key_i := inverse_key_i + 1;
end;
  inverse_key_adj := adjugate(key);
  inverse_key_tmp := scalar_matrix_mult(inverse_key_det_inv, inverse_key_adj);
  inverse_key_res := matrix_mod(inverse_key_tmp);
  exit(inverse_key_res);
end;
function hill_encrypt(key: IntArrayArray; text: string): string;
var
  hill_encrypt_break_key: integer;
  hill_encrypt_processed: string;
  hill_encrypt_encrypted: string;
  hill_encrypt_i: integer;
  hill_encrypt_vec: array of integer;
  hill_encrypt_j: integer;
  hill_encrypt_enc_vec: IntArray;
  hill_encrypt_k: integer;
begin
  hill_encrypt_break_key := Length(key);
  hill_encrypt_processed := process_text(text, hill_encrypt_break_key);
  hill_encrypt_encrypted := '';
  hill_encrypt_i := 0;
  while hill_encrypt_i < Length(hill_encrypt_processed) do begin
  hill_encrypt_vec := [];
  hill_encrypt_j := 0;
  while hill_encrypt_j < hill_encrypt_break_key do begin
  hill_encrypt_vec := concat(hill_encrypt_vec, IntArray([replace_letters(hill_encrypt_processed[hill_encrypt_i + hill_encrypt_j+1])]));
  hill_encrypt_j := hill_encrypt_j + 1;
end;
  hill_encrypt_enc_vec := multiply_matrix_vector(key, hill_encrypt_vec);
  hill_encrypt_k := 0;
  while hill_encrypt_k < hill_encrypt_break_key do begin
  hill_encrypt_encrypted := hill_encrypt_encrypted + replace_digits(hill_encrypt_enc_vec[hill_encrypt_k]);
  hill_encrypt_k := hill_encrypt_k + 1;
end;
  hill_encrypt_i := hill_encrypt_i + hill_encrypt_break_key;
end;
  exit(hill_encrypt_encrypted);
end;
function hill_decrypt(key: IntArrayArray; text: string): string;
var
  hill_decrypt_break_key: integer;
  hill_decrypt_decrypt_key: IntArrayArray;
  hill_decrypt_processed: string;
  hill_decrypt_decrypted: string;
  hill_decrypt_i: integer;
  hill_decrypt_vec: array of integer;
  hill_decrypt_j: integer;
  hill_decrypt_dec_vec: IntArray;
  hill_decrypt_k: integer;
begin
  hill_decrypt_break_key := Length(key);
  hill_decrypt_decrypt_key := inverse_key(key);
  hill_decrypt_processed := process_text(text, hill_decrypt_break_key);
  hill_decrypt_decrypted := '';
  hill_decrypt_i := 0;
  while hill_decrypt_i < Length(hill_decrypt_processed) do begin
  hill_decrypt_vec := [];
  hill_decrypt_j := 0;
  while hill_decrypt_j < hill_decrypt_break_key do begin
  hill_decrypt_vec := concat(hill_decrypt_vec, IntArray([replace_letters(hill_decrypt_processed[hill_decrypt_i + hill_decrypt_j+1])]));
  hill_decrypt_j := hill_decrypt_j + 1;
end;
  hill_decrypt_dec_vec := multiply_matrix_vector(hill_decrypt_decrypt_key, hill_decrypt_vec);
  hill_decrypt_k := 0;
  while hill_decrypt_k < hill_decrypt_break_key do begin
  hill_decrypt_decrypted := hill_decrypt_decrypted + replace_digits(hill_decrypt_dec_vec[hill_decrypt_k]);
  hill_decrypt_k := hill_decrypt_k + 1;
end;
  hill_decrypt_i := hill_decrypt_i + hill_decrypt_break_key;
end;
  exit(hill_decrypt_decrypted);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  KEY_STRING := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  key := [[2, 5], [1, 6]];
  writeln(hill_encrypt(key, 'testing hill cipher'));
  writeln(hill_encrypt(key, 'hello'));
  writeln(hill_decrypt(key, 'WHXYJOLM9C6XT085LL'));
  writeln(hill_decrypt(key, '85FF00'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
