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
  CHARS: array of string;
  CODES: array of string;
  msg: string;
  enc: string;
  dec: string;
  target: string;
  xs: StrArray;
  c: string;
  s: string;
  message: string;
function to_upper_char(c: string): string; forward;
function to_upper(s: string): string; forward;
function index_of(xs: StrArray; target: string): integer; forward;
function encrypt(message: string): string; forward;
function split_spaces(s: string): StrArray; forward;
function decrypt(message: string): string; forward;
function to_upper_char(c: string): string;
begin
  if c = 'a' then begin
  exit('A');
end;
  if c = 'b' then begin
  exit('B');
end;
  if c = 'c' then begin
  exit('C');
end;
  if c = 'd' then begin
  exit('D');
end;
  if c = 'e' then begin
  exit('E');
end;
  if c = 'f' then begin
  exit('F');
end;
  if c = 'g' then begin
  exit('G');
end;
  if c = 'h' then begin
  exit('H');
end;
  if c = 'i' then begin
  exit('I');
end;
  if c = 'j' then begin
  exit('J');
end;
  if c = 'k' then begin
  exit('K');
end;
  if c = 'l' then begin
  exit('L');
end;
  if c = 'm' then begin
  exit('M');
end;
  if c = 'n' then begin
  exit('N');
end;
  if c = 'o' then begin
  exit('O');
end;
  if c = 'p' then begin
  exit('P');
end;
  if c = 'q' then begin
  exit('Q');
end;
  if c = 'r' then begin
  exit('R');
end;
  if c = 's' then begin
  exit('S');
end;
  if c = 't' then begin
  exit('T');
end;
  if c = 'u' then begin
  exit('U');
end;
  if c = 'v' then begin
  exit('V');
end;
  if c = 'w' then begin
  exit('W');
end;
  if c = 'x' then begin
  exit('X');
end;
  if c = 'y' then begin
  exit('Y');
end;
  if c = 'z' then begin
  exit('Z');
end;
  exit(c);
end;
function to_upper(s: string): string;
var
  to_upper_res: string;
  to_upper_i: integer;
begin
  to_upper_res := '';
  to_upper_i := 0;
  while to_upper_i < Length(s) do begin
  to_upper_res := to_upper_res + to_upper_char(s[to_upper_i+1]);
  to_upper_i := to_upper_i + 1;
end;
  exit(to_upper_res);
end;
function index_of(xs: StrArray; target: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(xs) do begin
  if xs[index_of_i] = target then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function encrypt(message: string): string;
var
  encrypt_msg: string;
  encrypt_res: string;
  encrypt_i: integer;
  encrypt_c: string;
  encrypt_idx: integer;
begin
  encrypt_msg := to_upper(message);
  encrypt_res := '';
  encrypt_i := 0;
  while encrypt_i < Length(encrypt_msg) do begin
  encrypt_c := encrypt_msg[encrypt_i+1];
  encrypt_idx := index_of(CHARS, encrypt_c);
  if encrypt_idx >= 0 then begin
  if encrypt_res <> '' then begin
  encrypt_res := encrypt_res + ' ';
end;
  encrypt_res := encrypt_res + CODES[encrypt_idx];
end;
  encrypt_i := encrypt_i + 1;
end;
  exit(encrypt_res);
end;
function split_spaces(s: string): StrArray;
var
  split_spaces_res: array of string;
  split_spaces_current: string;
  split_spaces_i: integer;
  split_spaces_ch: string;
begin
  split_spaces_res := [];
  split_spaces_current := '';
  split_spaces_i := 0;
  while split_spaces_i < Length(s) do begin
  split_spaces_ch := s[split_spaces_i+1];
  if split_spaces_ch = ' ' then begin
  if split_spaces_current <> '' then begin
  split_spaces_res := concat(split_spaces_res, StrArray([split_spaces_current]));
  split_spaces_current := '';
end;
end else begin
  split_spaces_current := split_spaces_current + split_spaces_ch;
end;
  split_spaces_i := split_spaces_i + 1;
end;
  if split_spaces_current <> '' then begin
  split_spaces_res := concat(split_spaces_res, StrArray([split_spaces_current]));
end;
  exit(split_spaces_res);
end;
function decrypt(message: string): string;
var
  decrypt_parts: StrArray;
  decrypt_res: string;
  decrypt_code: string;
  decrypt_idx: integer;
begin
  decrypt_parts := split_spaces(message);
  decrypt_res := '';
  for decrypt_code in decrypt_parts do begin
  decrypt_idx := index_of(CODES, decrypt_code);
  if decrypt_idx >= 0 then begin
  decrypt_res := decrypt_res + CHARS[decrypt_idx];
end;
end;
  exit(decrypt_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  CHARS := ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '&', '@', ':', ',', '.', '''', '"', '?', '/', '=', '+', '-', '(', ')', '!', ' '];
  CODES := ['.-', '-...', '-.-.', '-..', '.', '..-.', '--.', '....', '..', '.---', '-.-', '.-..', '--', '-.', '---', '.--.', '--.-', '.-.', '...', '-', '..-', '...-', '.--', '-..-', '-.--', '--..', '.----', '..---', '...--', '....-', '.....', '-....', '--...', '---..', '----.', '-----', '.-...', '.--.-.', '---...', '--..--', '.-.-.-', '.----.', '.-..-.', '..--..', '-..-.', '-...-', '.-.-.', '-....-', '-.--.', '-.--.-', '-.-.--', '/'];
  msg := 'Morse code here!';
  writeln(msg);
  enc := encrypt(msg);
  writeln(enc);
  dec := decrypt(enc);
  writeln(dec);
  writeln(encrypt('Sos!'));
  writeln(decrypt('... --- ... -.-.--'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
