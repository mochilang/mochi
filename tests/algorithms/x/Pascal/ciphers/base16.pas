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
  hex_value_j: integer;
  example1: array of integer;
  example2: array of integer;
  data: string;
function base16_encode(data: IntArray): string; forward;
function base16_decode(data: string): IntArray; forward;
function base16_encode(data: IntArray): string;
var
  base16_encode_digits: string;
  base16_encode_res: string;
  base16_encode_i: integer;
  base16_encode_b: integer;
  base16_encode_hi: integer;
  base16_encode_lo: integer;
begin
  base16_encode_digits := '0123456789ABCDEF';
  base16_encode_res := '';
  base16_encode_i := 0;
  while base16_encode_i < Length(data) do begin
  base16_encode_b := data[base16_encode_i];
  if (base16_encode_b < 0) or (base16_encode_b > 255) then begin
  panic('byte out of range');
end;
  base16_encode_hi := base16_encode_b div 16;
  base16_encode_lo := base16_encode_b mod 16;
  base16_encode_res := (base16_encode_res + copy(base16_encode_digits, base16_encode_hi+1, (base16_encode_hi + 1 - (base16_encode_hi)))) + copy(base16_encode_digits, base16_encode_lo+1, (base16_encode_lo + 1 - (base16_encode_lo)));
  base16_encode_i := base16_encode_i + 1;
end;
  exit(base16_encode_res);
end;
function base16_decode(data: string): IntArray;
var
  base16_decode_digits: string;
  base16_decode_out: array of integer;
  base16_decode_i: integer;
  base16_decode_hi_char: string;
  base16_decode_lo_char: string;
  base16_decode_hi: integer;
  base16_decode_lo: integer;
  base16_decode_ch: string;
  function hex_value(base16_decode_ch: string): integer;
begin
  hex_value_j := 0;
  while hex_value_j < 16 do begin
  if copy(base16_decode_digits, hex_value_j+1, (hex_value_j + 1 - (hex_value_j))) = base16_decode_ch then begin
  exit(hex_value_j);
end;
  hex_value_j := hex_value_j + 1;
end;
  exit(-1);
end;
begin
  base16_decode_digits := '0123456789ABCDEF';
  if (Length(data) mod 2) <> 0 then begin
  panic('Base16 encoded data is invalid: Data does not have an even number of hex digits.');
end;
  base16_decode_out := [];
  base16_decode_i := 0;
  while base16_decode_i < Length(data) do begin
  base16_decode_hi_char := copy(data, base16_decode_i+1, (base16_decode_i + 1 - (base16_decode_i)));
  base16_decode_lo_char := copy(data, base16_decode_i + 1+1, (base16_decode_i + 2 - (base16_decode_i + 1)));
  base16_decode_hi := hex_value(base16_decode_hi_char);
  base16_decode_lo := hex_value(base16_decode_lo_char);
  if (base16_decode_hi < 0) or (base16_decode_lo < 0) then begin
  panic('Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters.');
end;
  base16_decode_out := concat(base16_decode_out, IntArray([(base16_decode_hi * 16) + base16_decode_lo]));
  base16_decode_i := base16_decode_i + 2;
end;
  exit(base16_decode_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33];
  example2 := [72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68, 33];
  writeln(base16_encode(example1));
  writeln(base16_encode(example2));
  writeln(base16_encode([]));
  writeln(list_int_to_str(base16_decode('48656C6C6F20576F726C6421')));
  writeln(list_int_to_str(base16_decode('48454C4C4F20574F524C4421')));
  writeln(list_int_to_str(base16_decode('')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
