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
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
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
  ch: string;
  s: string;
  encoded: IntArray;
  n: integer;
  plain: string;
function indexOf(s: string; ch: string): integer; forward;
function charToNum(ch: string): integer; forward;
function numToChar(n: integer): string; forward;
function encode(plain: string): IntArray; forward;
function decode(encoded: IntArray): string; forward;
procedure main(); forward;
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
function charToNum(ch: string): integer;
var
  charToNum_letters: string;
  charToNum_idx: integer;
begin
  charToNum_letters := 'abcdefghijklmnopqrstuvwxyz';
  charToNum_idx := indexOf(charToNum_letters, ch);
  if charToNum_idx >= 0 then begin
  exit(charToNum_idx + 1);
end;
  exit(0);
end;
function numToChar(n: integer): string;
var
  numToChar_letters: string;
begin
  numToChar_letters := 'abcdefghijklmnopqrstuvwxyz';
  if (n >= 1) and (n <= 26) then begin
  exit(copy(numToChar_letters, n - 1+1, (n - (n - 1))));
end;
  exit('?');
end;
function encode(plain: string): IntArray;
var
  encode_res: array of integer;
  encode_i: integer;
  encode_ch: string;
  encode_val: integer;
begin
  encode_res := [];
  encode_i := 0;
  while encode_i < Length(plain) do begin
  encode_ch := LowerCase(copy(plain, encode_i+1, (encode_i + 1 - (encode_i))));
  encode_val := charToNum(encode_ch);
  if encode_val > 0 then begin
  encode_res := concat(encode_res, IntArray([encode_val]));
end;
  encode_i := encode_i + 1;
end;
  exit(encode_res);
end;
function decode(encoded: IntArray): string;
var
  decode_out: string;
  decode_n: integer;
begin
  decode_out := '';
  for decode_n in encoded do begin
  decode_out := decode_out + numToChar(decode_n);
end;
  exit(decode_out);
end;
procedure main();
var
  main_text: string;
  main_enc: IntArray;
begin
  writeln('-> ');
  main_text := LowerCase(_input());
  main_enc := encode(main_text);
  writeln('Encoded: ' + list_int_to_str(main_enc));
  writeln('Decoded: ' + decode(main_enc));
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
