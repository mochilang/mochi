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
  default_alphabet: string;
  key: integer;
  alphabet: string;
  input_string: string;
  ch: string;
  s: string;
function index_of(s: string; ch: string): integer; forward;
function encrypt(input_string: string; key: integer; alphabet: string): string; forward;
function decrypt(input_string: string; key: integer; alphabet: string): string; forward;
function brute_force(input_string: string; alphabet: string): StrArray; forward;
procedure main(); forward;
function index_of(s: string; ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if copy(s, index_of_i+1, (index_of_i + 1 - (index_of_i))) = ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function encrypt(input_string: string; key: integer; alphabet: string): string;
var
  encrypt_result_: string;
  encrypt_i: integer;
  encrypt_n: integer;
  encrypt_ch: string;
  encrypt_idx: integer;
  encrypt_new_key: integer;
begin
  encrypt_result_ := '';
  encrypt_i := 0;
  encrypt_n := Length(alphabet);
  while encrypt_i < Length(input_string) do begin
  encrypt_ch := copy(input_string, encrypt_i+1, (encrypt_i + 1 - (encrypt_i)));
  encrypt_idx := index_of(alphabet, encrypt_ch);
  if encrypt_idx < 0 then begin
  encrypt_result_ := encrypt_result_ + encrypt_ch;
end else begin
  encrypt_new_key := (encrypt_idx + key) mod encrypt_n;
  if encrypt_new_key < 0 then begin
  encrypt_new_key := encrypt_new_key + encrypt_n;
end;
  encrypt_result_ := encrypt_result_ + copy(alphabet, encrypt_new_key+1, (encrypt_new_key + 1 - (encrypt_new_key)));
end;
  encrypt_i := encrypt_i + 1;
end;
  exit(encrypt_result_);
end;
function decrypt(input_string: string; key: integer; alphabet: string): string;
var
  decrypt_result_: string;
  decrypt_i: integer;
  decrypt_n: integer;
  decrypt_ch: string;
  decrypt_idx: integer;
  decrypt_new_key: integer;
begin
  decrypt_result_ := '';
  decrypt_i := 0;
  decrypt_n := Length(alphabet);
  while decrypt_i < Length(input_string) do begin
  decrypt_ch := copy(input_string, decrypt_i+1, (decrypt_i + 1 - (decrypt_i)));
  decrypt_idx := index_of(alphabet, decrypt_ch);
  if decrypt_idx < 0 then begin
  decrypt_result_ := decrypt_result_ + decrypt_ch;
end else begin
  decrypt_new_key := (decrypt_idx - key) mod decrypt_n;
  if decrypt_new_key < 0 then begin
  decrypt_new_key := decrypt_new_key + decrypt_n;
end;
  decrypt_result_ := decrypt_result_ + copy(alphabet, decrypt_new_key+1, (decrypt_new_key + 1 - (decrypt_new_key)));
end;
  decrypt_i := decrypt_i + 1;
end;
  exit(decrypt_result_);
end;
function brute_force(input_string: string; alphabet: string): StrArray;
var
  brute_force_results: array of string;
  brute_force_key: integer;
  brute_force_n: integer;
  brute_force_message: string;
begin
  brute_force_results := [];
  brute_force_key := 1;
  brute_force_n := Length(alphabet);
  while brute_force_key <= brute_force_n do begin
  brute_force_message := decrypt(input_string, brute_force_key, alphabet);
  brute_force_results := concat(brute_force_results, StrArray([brute_force_message]));
  brute_force_key := brute_force_key + 1;
end;
  exit(brute_force_results);
end;
procedure main();
var
  main_alpha: string;
  main_enc: string;
  main_dec: string;
  main_brute: StrArray;
begin
  main_alpha := default_alphabet;
  main_enc := encrypt('The quick brown fox jumps over the lazy dog', 8, main_alpha);
  writeln(main_enc);
  main_dec := decrypt(main_enc, 8, main_alpha);
  writeln(main_dec);
  main_brute := brute_force('jFyuMy xIH''N vLONy zILwy Gy!', main_alpha);
  writeln(main_brute[19]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  default_alphabet := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
