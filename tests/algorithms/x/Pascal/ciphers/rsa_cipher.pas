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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  BYTE_SIZE: integer;
  base: integer;
  exponent: integer;
  modulus: integer;
  ch: string;
  e: integer;
  d: integer;
  code: integer;
  message: string;
  block_ints: IntArray;
  blocks: IntArray;
  exp: integer;
  block_size: integer;
  message_length: integer;
  n: integer;
function pow_int(base: integer; exp: integer): integer; forward;
function mod_pow(base: integer; exponent: integer; modulus: integer): integer; forward;
function ord(ch: string): integer; forward;
function chr(code: integer): string; forward;
function get_blocks_from_text(message: string; block_size: integer): IntArray; forward;
function get_text_from_blocks(block_ints: IntArray; message_length: integer; block_size: integer): string; forward;
function encrypt_message(message: string; n: integer; e: integer; block_size: integer): IntArray; forward;
function decrypt_message(blocks: IntArray; message_length: integer; n: integer; d: integer; block_size: integer): string; forward;
procedure main(); forward;
function pow_int(base: integer; exp: integer): integer;
var
  pow_int_result_: integer;
  pow_int_i: integer;
begin
  pow_int_result_ := 1;
  pow_int_i := 0;
  while pow_int_i < exp do begin
  pow_int_result_ := pow_int_result_ * base;
  pow_int_i := pow_int_i + 1;
end;
  exit(pow_int_result_);
end;
function mod_pow(base: integer; exponent: integer; modulus: integer): integer;
var
  mod_pow_result_: integer;
  mod_pow_b: integer;
  mod_pow_e: integer;
begin
  mod_pow_result_ := 1;
  mod_pow_b := base mod modulus;
  mod_pow_e := exponent;
  while mod_pow_e > 0 do begin
  if (mod_pow_e mod 2) = 1 then begin
  mod_pow_result_ := (mod_pow_result_ * mod_pow_b) mod modulus;
end;
  mod_pow_e := mod_pow_e div 2;
  mod_pow_b := (mod_pow_b * mod_pow_b) mod modulus;
end;
  exit(mod_pow_result_);
end;
function ord(ch: string): integer;
begin
  if ch = ' ' then begin
  exit(32);
end;
  if ch = 'a' then begin
  exit(97);
end;
  if ch = 'b' then begin
  exit(98);
end;
  if ch = 'c' then begin
  exit(99);
end;
  if ch = 'd' then begin
  exit(100);
end;
  if ch = 'e' then begin
  exit(101);
end;
  if ch = 'f' then begin
  exit(102);
end;
  if ch = 'g' then begin
  exit(103);
end;
  if ch = 'h' then begin
  exit(104);
end;
  if ch = 'i' then begin
  exit(105);
end;
  if ch = 'j' then begin
  exit(106);
end;
  if ch = 'k' then begin
  exit(107);
end;
  if ch = 'l' then begin
  exit(108);
end;
  if ch = 'm' then begin
  exit(109);
end;
  if ch = 'n' then begin
  exit(110);
end;
  if ch = 'o' then begin
  exit(111);
end;
  if ch = 'p' then begin
  exit(112);
end;
  if ch = 'q' then begin
  exit(113);
end;
  if ch = 'r' then begin
  exit(114);
end;
  if ch = 's' then begin
  exit(115);
end;
  if ch = 't' then begin
  exit(116);
end;
  if ch = 'u' then begin
  exit(117);
end;
  if ch = 'v' then begin
  exit(118);
end;
  if ch = 'w' then begin
  exit(119);
end;
  if ch = 'x' then begin
  exit(120);
end;
  if ch = 'y' then begin
  exit(121);
end;
  if ch = 'z' then begin
  exit(122);
end;
  exit(0);
end;
function chr(code: integer): string;
begin
  if code = 32 then begin
  exit(' ');
end;
  if code = 97 then begin
  exit('a');
end;
  if code = 98 then begin
  exit('b');
end;
  if code = 99 then begin
  exit('c');
end;
  if code = 100 then begin
  exit('d');
end;
  if code = 101 then begin
  exit('e');
end;
  if code = 102 then begin
  exit('f');
end;
  if code = 103 then begin
  exit('g');
end;
  if code = 104 then begin
  exit('h');
end;
  if code = 105 then begin
  exit('i');
end;
  if code = 106 then begin
  exit('j');
end;
  if code = 107 then begin
  exit('k');
end;
  if code = 108 then begin
  exit('l');
end;
  if code = 109 then begin
  exit('m');
end;
  if code = 110 then begin
  exit('n');
end;
  if code = 111 then begin
  exit('o');
end;
  if code = 112 then begin
  exit('p');
end;
  if code = 113 then begin
  exit('q');
end;
  if code = 114 then begin
  exit('r');
end;
  if code = 115 then begin
  exit('s');
end;
  if code = 116 then begin
  exit('t');
end;
  if code = 117 then begin
  exit('u');
end;
  if code = 118 then begin
  exit('v');
end;
  if code = 119 then begin
  exit('w');
end;
  if code = 120 then begin
  exit('x');
end;
  if code = 121 then begin
  exit('y');
end;
  if code = 122 then begin
  exit('z');
end;
  exit('');
end;
function get_blocks_from_text(message: string; block_size: integer): IntArray;
var
  get_blocks_from_text_block_ints: array of integer;
  get_blocks_from_text_block_start: integer;
  get_blocks_from_text_block_int: integer;
  get_blocks_from_text_i: integer;
begin
  get_blocks_from_text_block_ints := [];
  get_blocks_from_text_block_start := 0;
  while get_blocks_from_text_block_start < Length(message) do begin
  get_blocks_from_text_block_int := 0;
  get_blocks_from_text_i := get_blocks_from_text_block_start;
  while (get_blocks_from_text_i < (get_blocks_from_text_block_start + block_size)) and (get_blocks_from_text_i < Length(message)) do begin
  get_blocks_from_text_block_int := get_blocks_from_text_block_int + (ord(message[get_blocks_from_text_i+1]) * pow_int(BYTE_SIZE, get_blocks_from_text_i - get_blocks_from_text_block_start));
  get_blocks_from_text_i := get_blocks_from_text_i + 1;
end;
  get_blocks_from_text_block_ints := concat(get_blocks_from_text_block_ints, IntArray([get_blocks_from_text_block_int]));
  get_blocks_from_text_block_start := get_blocks_from_text_block_start + block_size;
end;
  exit(get_blocks_from_text_block_ints);
end;
function get_text_from_blocks(block_ints: IntArray; message_length: integer; block_size: integer): string;
var
  get_text_from_blocks_message: string;
  get_text_from_blocks_block_int: integer;
  get_text_from_blocks_block: integer;
  get_text_from_blocks_i: integer;
  get_text_from_blocks_block_message: string;
  get_text_from_blocks_ascii_number: integer;
begin
  get_text_from_blocks_message := '';
  for get_text_from_blocks_block_int in block_ints do begin
  get_text_from_blocks_block := get_text_from_blocks_block_int;
  get_text_from_blocks_i := block_size - 1;
  get_text_from_blocks_block_message := '';
  while get_text_from_blocks_i >= 0 do begin
  if (Length(get_text_from_blocks_message) + get_text_from_blocks_i) < message_length then begin
  get_text_from_blocks_ascii_number := get_text_from_blocks_block div pow_int(BYTE_SIZE, get_text_from_blocks_i);
  get_text_from_blocks_block := get_text_from_blocks_block mod pow_int(BYTE_SIZE, get_text_from_blocks_i);
  get_text_from_blocks_block_message := chr(get_text_from_blocks_ascii_number) + get_text_from_blocks_block_message;
end;
  get_text_from_blocks_i := get_text_from_blocks_i - 1;
end;
  get_text_from_blocks_message := get_text_from_blocks_message + get_text_from_blocks_block_message;
end;
  exit(get_text_from_blocks_message);
end;
function encrypt_message(message: string; n: integer; e: integer; block_size: integer): IntArray;
var
  encrypt_message_encrypted: array of integer;
  encrypt_message_blocks: IntArray;
  encrypt_message_block: integer;
begin
  encrypt_message_encrypted := [];
  encrypt_message_blocks := get_blocks_from_text(message, block_size);
  for encrypt_message_block in encrypt_message_blocks do begin
  encrypt_message_encrypted := concat(encrypt_message_encrypted, IntArray([mod_pow(encrypt_message_block, e, n)]));
end;
  exit(encrypt_message_encrypted);
end;
function decrypt_message(blocks: IntArray; message_length: integer; n: integer; d: integer; block_size: integer): string;
var
  decrypt_message_decrypted_blocks: array of integer;
  decrypt_message_block: integer;
  decrypt_message_message: string;
  decrypt_message_num: integer;
begin
  decrypt_message_decrypted_blocks := [];
  for decrypt_message_block in blocks do begin
  decrypt_message_decrypted_blocks := concat(decrypt_message_decrypted_blocks, IntArray([mod_pow(decrypt_message_block, d, n)]));
end;
  decrypt_message_message := '';
  for decrypt_message_num in decrypt_message_decrypted_blocks do begin
  decrypt_message_message := decrypt_message_message + chr(decrypt_message_num);
end;
  exit(decrypt_message_message);
end;
procedure main();
var
  main_message: string;
  main_n: integer;
  main_e: integer;
  main_d: integer;
  main_block_size: integer;
  main_encrypted: IntArray;
  main_decrypted: string;
begin
  main_message := 'hello world';
  main_n := 3233;
  main_e := 17;
  main_d := 2753;
  main_block_size := 1;
  main_encrypted := encrypt_message(main_message, main_n, main_e, main_block_size);
  writeln(list_int_to_str(main_encrypted));
  main_decrypted := decrypt_message(main_encrypted, Length(main_message), main_n, main_d, main_block_size);
  writeln(main_decrypted);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  BYTE_SIZE := 256;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
