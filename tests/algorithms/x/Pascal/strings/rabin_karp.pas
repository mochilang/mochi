{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  alphabet_size: integer;
  modulus: integer;
function index_of_char(index_of_char_s: string; index_of_char_ch: string): integer; forward;
function ord_(ord__ch: string): integer; forward;
function rabin_karp(rabin_karp_pattern: string; rabin_karp_text: string): boolean; forward;
procedure test_rabin_karp(); forward;
function index_of_char(index_of_char_s: string; index_of_char_ch: string): integer;
var
  index_of_char_i: integer;
begin
  index_of_char_i := 0;
  while index_of_char_i < Length(index_of_char_s) do begin
  if index_of_char_s[index_of_char_i+1] = index_of_char_ch then begin
  exit(index_of_char_i);
end;
  index_of_char_i := index_of_char_i + 1;
end;
  exit(-1);
end;
function ord_(ord__ch: string): integer;
var
  ord__upper: string;
  ord__lower: string;
  ord__digits: string;
  ord__idx: integer;
begin
  ord__upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ord__lower := 'abcdefghijklmnopqrstuvwxyz';
  ord__digits := '0123456789';
  ord__idx := index_of_char(ord__upper, ord__ch);
  if ord__idx >= 0 then begin
  exit(65 + ord__idx);
end;
  ord__idx := index_of_char(ord__lower, ord__ch);
  if ord__idx >= 0 then begin
  exit(97 + ord__idx);
end;
  ord__idx := index_of_char(ord__digits, ord__ch);
  if ord__idx >= 0 then begin
  exit(48 + ord__idx);
end;
  if ord__ch = 'ü' then begin
  exit(252);
end;
  if ord__ch = 'Ü' then begin
  exit(220);
end;
  if ord__ch = ' ' then begin
  exit(32);
end;
  exit(0);
end;
function rabin_karp(rabin_karp_pattern: string; rabin_karp_text: string): boolean;
var
  rabin_karp_p_len: integer;
  rabin_karp_t_len: integer;
  rabin_karp_p_hash: integer;
  rabin_karp_t_hash: integer;
  rabin_karp_modulus_power: integer;
  rabin_karp_i: integer;
  rabin_karp_j: integer;
begin
  rabin_karp_p_len := Length(rabin_karp_pattern);
  rabin_karp_t_len := Length(rabin_karp_text);
  if rabin_karp_p_len > rabin_karp_t_len then begin
  exit(false);
end;
  rabin_karp_p_hash := 0;
  rabin_karp_t_hash := 0;
  rabin_karp_modulus_power := 1;
  rabin_karp_i := 0;
  while rabin_karp_i < rabin_karp_p_len do begin
  rabin_karp_p_hash := (ord_(rabin_karp_pattern[rabin_karp_i+1]) + (rabin_karp_p_hash * alphabet_size)) mod modulus;
  rabin_karp_t_hash := (ord_(rabin_karp_text[rabin_karp_i+1]) + (rabin_karp_t_hash * alphabet_size)) mod modulus;
  if rabin_karp_i <> (rabin_karp_p_len - 1) then begin
  rabin_karp_modulus_power := (rabin_karp_modulus_power * alphabet_size) mod modulus;
end;
  rabin_karp_i := rabin_karp_i + 1;
end;
  rabin_karp_j := 0;
  while rabin_karp_j <= (rabin_karp_t_len - rabin_karp_p_len) do begin
  if (rabin_karp_t_hash = rabin_karp_p_hash) and (copy(rabin_karp_text, rabin_karp_j+1, (rabin_karp_j + rabin_karp_p_len - (rabin_karp_j))) = rabin_karp_pattern) then begin
  exit(true);
end;
  if rabin_karp_j = (rabin_karp_t_len - rabin_karp_p_len) then begin
  rabin_karp_j := rabin_karp_j + 1;
  continue;
end;
  rabin_karp_t_hash := (((rabin_karp_t_hash - (ord_(rabin_karp_text[rabin_karp_j+1]) * rabin_karp_modulus_power)) * alphabet_size) + ord_(rabin_karp_text[rabin_karp_j + rabin_karp_p_len+1])) mod modulus;
  if rabin_karp_t_hash < 0 then begin
  rabin_karp_t_hash := rabin_karp_t_hash + modulus;
end;
  rabin_karp_j := rabin_karp_j + 1;
end;
  exit(false);
end;
procedure test_rabin_karp();
var
  test_rabin_karp_pattern1: string;
  test_rabin_karp_text1: string;
  test_rabin_karp_text2: string;
  test_rabin_karp_pattern2: string;
  test_rabin_karp_text3: string;
  test_rabin_karp_pattern3: string;
  test_rabin_karp_text4: string;
  test_rabin_karp_pattern4: string;
  test_rabin_karp_text5: string;
  test_rabin_karp_pattern5: string;
  test_rabin_karp_text6: string;
  test_rabin_karp_pattern6: string;
begin
  test_rabin_karp_pattern1 := 'abc1abc12';
  test_rabin_karp_text1 := 'alskfjaldsabc1abc1abc12k23adsfabcabc';
  test_rabin_karp_text2 := 'alskfjaldsk23adsfabcabc';
  if not rabin_karp(test_rabin_karp_pattern1, test_rabin_karp_text1) or rabin_karp(test_rabin_karp_pattern1, test_rabin_karp_text2) then begin
  writeln('Failure');
  exit();
end;
  test_rabin_karp_pattern2 := 'ABABX';
  test_rabin_karp_text3 := 'ABABZABABYABABX';
  if not rabin_karp(test_rabin_karp_pattern2, test_rabin_karp_text3) then begin
  writeln('Failure');
  exit();
end;
  test_rabin_karp_pattern3 := 'AAAB';
  test_rabin_karp_text4 := 'ABAAAAAB';
  if not rabin_karp(test_rabin_karp_pattern3, test_rabin_karp_text4) then begin
  writeln('Failure');
  exit();
end;
  test_rabin_karp_pattern4 := 'abcdabcy';
  test_rabin_karp_text5 := 'abcxabcdabxabcdabcdabcy';
  if not rabin_karp(test_rabin_karp_pattern4, test_rabin_karp_text5) then begin
  writeln('Failure');
  exit();
end;
  test_rabin_karp_pattern5 := 'Lü';
  test_rabin_karp_text6 := 'Lüsai';
  if not rabin_karp(test_rabin_karp_pattern5, test_rabin_karp_text6) then begin
  writeln('Failure');
  exit();
end;
  test_rabin_karp_pattern6 := 'Lue';
  if rabin_karp(test_rabin_karp_pattern6, test_rabin_karp_text6) then begin
  writeln('Failure');
  exit();
end;
  writeln('Success.');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  alphabet_size := 256;
  modulus := 1000003;
  test_rabin_karp();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
