{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function pow2(pow2_n: integer): integer; forward;
function bit_and(bit_and_a: integer; bit_and_b: integer): integer; forward;
function bit_or(bit_or_a: integer; bit_or_b: integer): integer; forward;
function char_to_index(char_to_index_ch: string): integer; forward;
function bitap_string_match(bitap_string_match_text: string; bitap_string_match_pattern: string): integer; forward;
procedure main(); forward;
function pow2(pow2_n: integer): integer;
var
  pow2_res: integer;
  pow2_i: integer;
begin
  pow2_res := 1;
  pow2_i := 0;
  while pow2_i < pow2_n do begin
  pow2_res := pow2_res * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_res);
end;
function bit_and(bit_and_a: integer; bit_and_b: integer): integer;
var
  bit_and_x: integer;
  bit_and_y: integer;
  bit_and_res: integer;
  bit_and_bit: integer;
begin
  bit_and_x := bit_and_a;
  bit_and_y := bit_and_b;
  bit_and_res := 0;
  bit_and_bit := 1;
  while (bit_and_x > 0) or (bit_and_y > 0) do begin
  if ((bit_and_x mod 2) = 1) and ((bit_and_y mod 2) = 1) then begin
  bit_and_res := bit_and_res + bit_and_bit;
end;
  bit_and_x := Trunc(bit_and_x div 2);
  bit_and_y := Trunc(bit_and_y div 2);
  bit_and_bit := bit_and_bit * 2;
end;
  exit(bit_and_res);
end;
function bit_or(bit_or_a: integer; bit_or_b: integer): integer;
var
  bit_or_x: integer;
  bit_or_y: integer;
  bit_or_res: integer;
  bit_or_bit: integer;
begin
  bit_or_x := bit_or_a;
  bit_or_y := bit_or_b;
  bit_or_res := 0;
  bit_or_bit := 1;
  while (bit_or_x > 0) or (bit_or_y > 0) do begin
  if ((bit_or_x mod 2) = 1) or ((bit_or_y mod 2) = 1) then begin
  bit_or_res := bit_or_res + bit_or_bit;
end;
  bit_or_x := Trunc(bit_or_x div 2);
  bit_or_y := Trunc(bit_or_y div 2);
  bit_or_bit := bit_or_bit * 2;
end;
  exit(bit_or_res);
end;
function char_to_index(char_to_index_ch: string): integer;
var
  char_to_index_letters: string;
  char_to_index_i: integer;
begin
  char_to_index_letters := 'abcdefghijklmnopqrstuvwxyz';
  char_to_index_i := 0;
  while char_to_index_i < Length(char_to_index_letters) do begin
  if copy(char_to_index_letters, char_to_index_i+1, (char_to_index_i + 1 - (char_to_index_i))) = char_to_index_ch then begin
  exit(char_to_index_i);
end;
  char_to_index_i := char_to_index_i + 1;
end;
  exit(26);
end;
function bitap_string_match(bitap_string_match_text: string; bitap_string_match_pattern: string): integer;
var
  bitap_string_match_m: integer;
  bitap_string_match_limit: integer;
  bitap_string_match_all_ones: integer;
  bitap_string_match_pattern_mask: array of integer;
  bitap_string_match_i: integer;
  bitap_string_match_ch: string;
  bitap_string_match_idx: integer;
  bitap_string_match_state: integer;
begin
  if bitap_string_match_pattern = '' then begin
  exit(0);
end;
  bitap_string_match_m := Length(bitap_string_match_pattern);
  if bitap_string_match_m > Length(bitap_string_match_text) then begin
  exit(-1);
end;
  bitap_string_match_limit := pow2(bitap_string_match_m + 1);
  bitap_string_match_all_ones := bitap_string_match_limit - 1;
  bitap_string_match_pattern_mask := [];
  bitap_string_match_i := 0;
  while bitap_string_match_i < 27 do begin
  bitap_string_match_pattern_mask := concat(bitap_string_match_pattern_mask, [bitap_string_match_all_ones]);
  bitap_string_match_i := bitap_string_match_i + 1;
end;
  bitap_string_match_i := 0;
  while bitap_string_match_i < bitap_string_match_m do begin
  bitap_string_match_ch := copy(bitap_string_match_pattern, bitap_string_match_i+1, (bitap_string_match_i + 1 - (bitap_string_match_i)));
  bitap_string_match_idx := char_to_index(bitap_string_match_ch);
  bitap_string_match_pattern_mask[bitap_string_match_idx] := bit_and(bitap_string_match_pattern_mask[bitap_string_match_idx], bitap_string_match_all_ones - pow2(bitap_string_match_i));
  bitap_string_match_i := bitap_string_match_i + 1;
end;
  bitap_string_match_state := bitap_string_match_all_ones - 1;
  bitap_string_match_i := 0;
  while bitap_string_match_i < Length(bitap_string_match_text) do begin
  bitap_string_match_ch := copy(bitap_string_match_text, bitap_string_match_i+1, (bitap_string_match_i + 1 - (bitap_string_match_i)));
  bitap_string_match_idx := char_to_index(bitap_string_match_ch);
  bitap_string_match_state := bit_or(bitap_string_match_state, bitap_string_match_pattern_mask[bitap_string_match_idx]);
  bitap_string_match_state := (bitap_string_match_state * 2) mod bitap_string_match_limit;
  if bit_and(bitap_string_match_state, pow2(bitap_string_match_m)) = 0 then begin
  exit((bitap_string_match_i - bitap_string_match_m) + 1);
end;
  bitap_string_match_i := bitap_string_match_i + 1;
end;
  exit(-1);
end;
procedure main();
begin
  writeln(IntToStr(bitap_string_match('abdabababc', 'ababc')));
  writeln(IntToStr(bitap_string_match('abdabababc', '')));
  writeln(IntToStr(bitap_string_match('abdabababc', 'c')));
  writeln(IntToStr(bitap_string_match('abdabababc', 'fofosdfo')));
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
  writeln('');
end.
