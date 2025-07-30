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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  adfgvx: string;
  alphabet: string;
function shuffleStr(s: string): string; forward;
function createPolybius(): StrArray; forward;
function createKey(n: integer): string; forward;
function orderKey(key: string): IntArray; forward;
function encrypt(polybius: StrArray; key: string; plainText: string): string; forward;
function indexOf(s: string; ch: string): integer; forward;
function decrypt(polybius: StrArray; key: string; cipherText: string): string; forward;
procedure main(); forward;
function shuffleStr(s: string): string;
var
  shuffleStr_arr: array of string;
  shuffleStr_i: integer;
  shuffleStr_j: integer;
  shuffleStr_k: integer;
  shuffleStr_tmp: string;
  shuffleStr_out: string;
begin
  shuffleStr_arr := [];
  shuffleStr_i := 0;
  while shuffleStr_i < Length(s) do begin
  shuffleStr_arr := concat(shuffleStr_arr, [copy(s, shuffleStr_i+1, (shuffleStr_i + 1 - (shuffleStr_i)))]);
  shuffleStr_i := shuffleStr_i + 1;
end;
  shuffleStr_j := Length(shuffleStr_arr) - 1;
  while shuffleStr_j > 0 do begin
  shuffleStr_k := _now() mod (shuffleStr_j + 1);
  shuffleStr_tmp := shuffleStr_arr[shuffleStr_j];
  shuffleStr_arr[shuffleStr_j] := shuffleStr_arr[shuffleStr_k];
  shuffleStr_arr[shuffleStr_k] := shuffleStr_tmp;
  shuffleStr_j := shuffleStr_j - 1;
end;
  shuffleStr_out := '';
  shuffleStr_i := 0;
  while shuffleStr_i < Length(shuffleStr_arr) do begin
  shuffleStr_out := shuffleStr_out + shuffleStr_arr[shuffleStr_i];
  shuffleStr_i := shuffleStr_i + 1;
end;
  exit(shuffleStr_out);
end;
function createPolybius(): StrArray;
var
  createPolybius_shuffled: string;
  createPolybius_labels: array of string;
  createPolybius_li: integer;
  createPolybius_p: array of string;
  createPolybius_i: integer;
  createPolybius_row: string;
  createPolybius_line: string;
  createPolybius_j: integer;
begin
  createPolybius_shuffled := shuffleStr(alphabet);
  createPolybius_labels := [];
  createPolybius_li := 0;
  while createPolybius_li < Length(adfgvx) do begin
  createPolybius_labels := concat(createPolybius_labels, [copy(adfgvx, createPolybius_li+1, (createPolybius_li + 1 - (createPolybius_li)))]);
  createPolybius_li := createPolybius_li + 1;
end;
  writeln('6 x 6 Polybius square:' + #10 + '');
  writeln('  | A D F G V X');
  writeln('---------------');
  createPolybius_p := [];
  createPolybius_i := 0;
  while createPolybius_i < 6 do begin
  createPolybius_row := copy(createPolybius_shuffled, createPolybius_i * 6+1, ((createPolybius_i + 1) * 6 - (createPolybius_i * 6)));
  createPolybius_p := concat(createPolybius_p, [createPolybius_row]);
  createPolybius_line := createPolybius_labels[createPolybius_i] + ' | ';
  createPolybius_j := 0;
  while createPolybius_j < 6 do begin
  createPolybius_line := (createPolybius_line + copy(createPolybius_row, createPolybius_j+1, (createPolybius_j + 1 - (createPolybius_j)))) + ' ';
  createPolybius_j := createPolybius_j + 1;
end;
  writeln(createPolybius_line);
  createPolybius_i := createPolybius_i + 1;
end;
  exit(createPolybius_p);
end;
function createKey(n: integer): string;
var
  createKey_pool: string;
  createKey_key: string;
  createKey_i: integer;
  createKey_idx: integer;
begin
  if (n < 7) or (n > 12) then begin
  writeln('Key should be within 7 and 12 letters long.');
end;
  createKey_pool := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  createKey_key := '';
  createKey_i := 0;
  while createKey_i < n do begin
  createKey_idx := _now() mod Length(createKey_pool);
  createKey_key := createKey_key + createKey_pool[createKey_idx+1];
  createKey_pool := copy(createKey_pool, 1, createKey_idx) + copy(createKey_pool, createKey_idx + 1+1, (Length(createKey_pool) - (createKey_idx + 1)));
  createKey_i := createKey_i + 1;
end;
  writeln('' + #10 + 'The key is ' + createKey_key);
  exit(createKey_key);
end;
function orderKey(key: string): IntArray;
var
  orderKey_pairs: integer;
  orderKey_i: integer;
  orderKey_n: integer;
  orderKey_m: integer;
  orderKey_j: integer;
  orderKey_tmp: integer;
  orderKey_res: array of integer;
begin
  orderKey_pairs := [];
  orderKey_i := 0;
  while orderKey_i < Length(key) do begin
  orderKey_pairs := concat(orderKey_pairs, [[copy(key, orderKey_i+1, (orderKey_i + 1 - (orderKey_i))), orderKey_i]]);
  orderKey_i := orderKey_i + 1;
end;
  orderKey_n := Length(orderKey_pairs);
  orderKey_m := 0;
  while orderKey_m < orderKey_n do begin
  orderKey_j := 0;
  while orderKey_j < (orderKey_n - 1) do begin
  if orderKey_pairs[orderKey_j][0] > orderKey_pairs[orderKey_j + 1][0] then begin
  orderKey_tmp := orderKey_pairs[orderKey_j];
  orderKey_pairs[orderKey_j] := orderKey_pairs[orderKey_j + 1];
  orderKey_pairs[orderKey_j + 1] := orderKey_tmp;
end;
  orderKey_j := orderKey_j + 1;
end;
  orderKey_m := orderKey_m + 1;
end;
  orderKey_res := [];
  orderKey_i := 0;
  while orderKey_i < orderKey_n do begin
  orderKey_res := concat(orderKey_res, [Trunc(orderKey_pairs[orderKey_i][1])]);
  orderKey_i := orderKey_i + 1;
end;
  exit(orderKey_res);
end;
function encrypt(polybius: StrArray; key: string; plainText: string): string;
var
  encrypt_labels: array of string;
  encrypt_li: integer;
  encrypt_temp: string;
  encrypt_i: integer;
  encrypt_r: integer;
  encrypt_c: integer;
  encrypt_colLen: integer;
  encrypt_table: array of StrArray;
  encrypt_rIdx: integer;
  encrypt_row: array of string;
  encrypt_j: integer;
  encrypt_idx: integer;
  encrypt_col: integer;
  encrypt_order: IntArray;
  encrypt_cols: array of string;
  encrypt_ci: integer;
  encrypt_colStr: string;
  encrypt_ri: integer;
  encrypt_result: string;
begin
  encrypt_labels := [];
  encrypt_li := 0;
  while encrypt_li < Length(adfgvx) do begin
  encrypt_labels := concat(encrypt_labels, [copy(adfgvx, encrypt_li+1, (encrypt_li + 1 - (encrypt_li)))]);
  encrypt_li := encrypt_li + 1;
end;
  encrypt_temp := '';
  encrypt_i := 0;
  while encrypt_i < Length(plainText) do begin
  encrypt_r := 0;
  while encrypt_r < 6 do begin
  encrypt_c := 0;
  while encrypt_c < 6 do begin
  if copy(polybius[encrypt_r], encrypt_c+1, (encrypt_c + 1 - (encrypt_c))) = copy(plainText, encrypt_i+1, (encrypt_i + 1 - (encrypt_i))) then begin
  encrypt_temp := (encrypt_temp + copy(encrypt_labels, encrypt_r, (encrypt_r + 1 - (encrypt_r)))) + copy(encrypt_labels, encrypt_c, (encrypt_c + 1 - (encrypt_c)));
end;
  encrypt_c := encrypt_c + 1;
end;
  encrypt_r := encrypt_r + 1;
end;
  encrypt_i := encrypt_i + 1;
end;
  encrypt_colLen := Length(encrypt_temp) div Length(key);
  if (Length(encrypt_temp) mod Length(key)) > 0 then begin
  encrypt_colLen := encrypt_colLen + 1;
end;
  encrypt_table := [];
  encrypt_rIdx := 0;
  while encrypt_rIdx < encrypt_colLen do begin
  encrypt_row := [];
  encrypt_j := 0;
  while encrypt_j < Length(key) do begin
  encrypt_row := concat(encrypt_row, ['']);
  encrypt_j := encrypt_j + 1;
end;
  encrypt_table := concat(encrypt_table, [encrypt_row]);
  encrypt_rIdx := encrypt_rIdx + 1;
end;
  encrypt_idx := 0;
  while encrypt_idx < Length(encrypt_temp) do begin
  encrypt_row := encrypt_idx div Length(key);
  encrypt_col := encrypt_idx mod Length(key);
  encrypt_table[encrypt_row][encrypt_col] := copy(encrypt_temp, encrypt_idx+1, (encrypt_idx + 1 - (encrypt_idx)));
  encrypt_idx := encrypt_idx + 1;
end;
  encrypt_order := orderKey(key);
  encrypt_cols := [];
  encrypt_ci := 0;
  while encrypt_ci < Length(key) do begin
  encrypt_colStr := '';
  encrypt_ri := 0;
  while encrypt_ri < encrypt_colLen do begin
  encrypt_colStr := encrypt_colStr + encrypt_table[encrypt_ri][encrypt_order[encrypt_ci]];
  encrypt_ri := encrypt_ri + 1;
end;
  encrypt_cols := concat(encrypt_cols, [encrypt_colStr]);
  encrypt_ci := encrypt_ci + 1;
end;
  encrypt_result := '';
  encrypt_ci := 0;
  while encrypt_ci < Length(encrypt_cols) do begin
  encrypt_result := encrypt_result + encrypt_cols[encrypt_ci];
  if encrypt_ci < (Length(encrypt_cols) - 1) then begin
  encrypt_result := encrypt_result + ' ';
end;
  encrypt_ci := encrypt_ci + 1;
end;
  exit(encrypt_result);
end;
function indexOf(s: string; ch: string): integer;
var
  indexOf_i: integer;
begin
  indexOf_i := 0;
  while indexOf_i < Length(s) do begin
  if copy(s, indexOf_i+1, (indexOf_i + 1 - (indexOf_i))) = ch then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(-1);
end;
function decrypt(polybius: StrArray; key: string; cipherText: string): string;
var
  decrypt_colStrs: array of string;
  decrypt_start: integer;
  decrypt_i: integer;
  decrypt_maxColLen: integer;
  decrypt_cols: array of StrArray;
  decrypt_s: string;
  decrypt_ls: array of string;
  decrypt_j: integer;
  decrypt_pad: array of string;
  decrypt_k: integer;
  decrypt_table: array of StrArray;
  decrypt_r: integer;
  decrypt_row: array of string;
  decrypt_c: integer;
  decrypt_order: IntArray;
  decrypt_temp: string;
  decrypt_plainText: string;
  decrypt_idx: integer;
  decrypt_rIdx: integer;
  decrypt_cIdx: integer;
begin
  decrypt_colStrs := [];
  decrypt_start := 0;
  decrypt_i := 0;
  while decrypt_i <= Length(cipherText) do begin
  if (decrypt_i = Length(cipherText)) or (cipherText[decrypt_i+1] = ' ') then begin
  decrypt_colStrs := concat(decrypt_colStrs, [copy(cipherText, decrypt_start+1, (decrypt_i - (decrypt_start)))]);
  decrypt_start := decrypt_i + 1;
end;
  decrypt_i := decrypt_i + 1;
end;
  decrypt_maxColLen := 0;
  decrypt_i := 0;
  while decrypt_i < Length(decrypt_colStrs) do begin
  if Length(decrypt_colStrs[decrypt_i]) > decrypt_maxColLen then begin
  decrypt_maxColLen := Length(decrypt_colStrs[decrypt_i]);
end;
  decrypt_i := decrypt_i + 1;
end;
  decrypt_cols := [];
  decrypt_i := 0;
  while decrypt_i < Length(decrypt_colStrs) do begin
  decrypt_s := decrypt_colStrs[decrypt_i];
  decrypt_ls := [];
  decrypt_j := 0;
  while decrypt_j < Length(decrypt_s) do begin
  decrypt_ls := concat(decrypt_ls, [copy(decrypt_s, decrypt_j+1, (decrypt_j + 1 - (decrypt_j)))]);
  decrypt_j := decrypt_j + 1;
end;
  if Length(decrypt_s) < decrypt_maxColLen then begin
  decrypt_pad := [];
  decrypt_k := 0;
  while decrypt_k < decrypt_maxColLen do begin
  if decrypt_k < Length(decrypt_ls) then begin
  decrypt_pad := concat(decrypt_pad, [decrypt_ls[decrypt_k]]);
end else begin
  decrypt_pad := concat(decrypt_pad, ['']);
end;
  decrypt_k := decrypt_k + 1;
end;
  decrypt_cols := concat(decrypt_cols, [decrypt_pad]);
end else begin
  decrypt_cols := concat(decrypt_cols, [decrypt_ls]);
end;
  decrypt_i := decrypt_i + 1;
end;
  decrypt_table := [];
  decrypt_r := 0;
  while decrypt_r < decrypt_maxColLen do begin
  decrypt_row := [];
  decrypt_c := 0;
  while decrypt_c < Length(key) do begin
  decrypt_row := concat(decrypt_row, ['']);
  decrypt_c := decrypt_c + 1;
end;
  decrypt_table := concat(decrypt_table, [decrypt_row]);
  decrypt_r := decrypt_r + 1;
end;
  decrypt_order := orderKey(key);
  decrypt_r := 0;
  while decrypt_r < decrypt_maxColLen do begin
  decrypt_c := 0;
  while decrypt_c < Length(key) do begin
  decrypt_table[decrypt_r][decrypt_order[decrypt_c]] := decrypt_cols[decrypt_c][decrypt_r];
  decrypt_c := decrypt_c + 1;
end;
  decrypt_r := decrypt_r + 1;
end;
  decrypt_temp := '';
  decrypt_r := 0;
  while decrypt_r < Length(decrypt_table) do begin
  decrypt_j := 0;
  while decrypt_j < Length(decrypt_table[decrypt_r]) do begin
  decrypt_temp := decrypt_temp + decrypt_table[decrypt_r][decrypt_j];
  decrypt_j := decrypt_j + 1;
end;
  decrypt_r := decrypt_r + 1;
end;
  decrypt_plainText := '';
  decrypt_idx := 0;
  while decrypt_idx < Length(decrypt_temp) do begin
  decrypt_rIdx := indexOf(adfgvx, copy(decrypt_temp, decrypt_idx+1, (decrypt_idx + 1 - (decrypt_idx))));
  decrypt_cIdx := indexOf(adfgvx, copy(decrypt_temp, decrypt_idx + 1+1, (decrypt_idx + 2 - (decrypt_idx + 1))));
  decrypt_plainText := decrypt_plainText + polybius[decrypt_rIdx][decrypt_cIdx+1];
  decrypt_idx := decrypt_idx + 2;
end;
  exit(decrypt_plainText);
end;
procedure main();
var
  main_plainText: string;
  main_polybius: StrArray;
  main_key: string;
  main_cipherText: string;
  main_plainText2: string;
begin
  main_plainText := 'ATTACKAT1200AM';
  main_polybius := createPolybius();
  main_key := createKey(9);
  writeln('' + #10 + 'Plaintext : ' + main_plainText);
  main_cipherText := encrypt(main_polybius, main_key, main_plainText);
  writeln('' + #10 + 'Encrypted : ' + main_cipherText);
  main_plainText2 := decrypt(main_polybius, main_key, main_cipherText);
  writeln('' + #10 + 'Decrypted : ' + main_plainText2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  adfgvx := 'ADFGVX';
  alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
