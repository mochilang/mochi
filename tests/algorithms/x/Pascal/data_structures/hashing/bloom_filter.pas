{$mode objfpc}
program Main;
uses SysUtils;
type Bloom = record
  size: integer;
  bits: array of integer;
end;
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
  ascii: string;
  b: Bloom;
  ch: string;
  items: StrArray;
  size: integer;
  value: string;
function makeBloom(size: integer; bits: IntArray): Bloom; forward;
function ord_(ch: string): integer; forward;
function new_bloom(size: integer): Bloom; forward;
function hash1(value: string; size: integer): integer; forward;
function hash2(value: string; size: integer): integer; forward;
function hash_positions(value: string; size: integer): IntArray; forward;
function bloom_add(b: Bloom; value: string): Bloom; forward;
function bloom_exists(b: Bloom; value: string): boolean; forward;
function bitstring(b: Bloom): string; forward;
function format_hash(b: Bloom; value: string): string; forward;
function estimated_error_rate(b: Bloom): real; forward;
function any_in(b: Bloom; items: StrArray): boolean; forward;
procedure main(); forward;
function makeBloom(size: integer; bits: IntArray): Bloom;
begin
  Result.size := size;
  Result.bits := bits;
end;
function ord_(ch: string): integer;
var
  ord__i: integer;
begin
  ord__i := 0;
  while ord__i < Length(ascii) do begin
  if copy(ascii, ord__i+1, (ord__i + 1 - (ord__i))) = ch then begin
  exit(32 + ord__i);
end;
  ord__i := ord__i + 1;
end;
  exit(0);
end;
function new_bloom(size: integer): Bloom;
var
  new_bloom_bits: array of integer;
  new_bloom_i: integer;
begin
  new_bloom_bits := [];
  new_bloom_i := 0;
  while new_bloom_i < size do begin
  new_bloom_bits := concat(new_bloom_bits, IntArray([0]));
  new_bloom_i := new_bloom_i + 1;
end;
  exit(makeBloom(size, new_bloom_bits));
end;
function hash1(value: string; size: integer): integer;
var
  hash1_h: integer;
  hash1_i: integer;
begin
  hash1_h := 0;
  hash1_i := 0;
  while hash1_i < Length(value) do begin
  hash1_h := ((hash1_h * 31) + ord_(copy(value, hash1_i+1, (hash1_i + 1 - (hash1_i))))) mod size;
  hash1_i := hash1_i + 1;
end;
  exit(hash1_h);
end;
function hash2(value: string; size: integer): integer;
var
  hash2_h: integer;
  hash2_i: integer;
begin
  hash2_h := 0;
  hash2_i := 0;
  while hash2_i < Length(value) do begin
  hash2_h := ((hash2_h * 131) + ord_(copy(value, hash2_i+1, (hash2_i + 1 - (hash2_i))))) mod size;
  hash2_i := hash2_i + 1;
end;
  exit(hash2_h);
end;
function hash_positions(value: string; size: integer): IntArray;
var
  hash_positions_h1: integer;
  hash_positions_h2: integer;
  hash_positions_res: array of integer;
begin
  hash_positions_h1 := hash1(value, size);
  hash_positions_h2 := hash2(value, size);
  hash_positions_res := [];
  hash_positions_res := concat(hash_positions_res, IntArray([hash_positions_h1]));
  hash_positions_res := concat(hash_positions_res, IntArray([hash_positions_h2]));
  exit(hash_positions_res);
end;
function bloom_add(b: Bloom; value: string): Bloom;
var
  bloom_add_pos: IntArray;
  bloom_add_bits: array of integer;
  bloom_add_i: integer;
  bloom_add_idx: integer;
begin
  bloom_add_pos := hash_positions(value, b.size);
  bloom_add_bits := b.bits;
  bloom_add_i := 0;
  while bloom_add_i < Length(bloom_add_pos) do begin
  bloom_add_idx := (b.size - 1) - bloom_add_pos[bloom_add_i];
  bloom_add_bits[bloom_add_idx] := 1;
  bloom_add_i := bloom_add_i + 1;
end;
  exit(makeBloom(b.size, bloom_add_bits));
end;
function bloom_exists(b: Bloom; value: string): boolean;
var
  bloom_exists_pos: IntArray;
  bloom_exists_i: integer;
  bloom_exists_idx: integer;
begin
  bloom_exists_pos := hash_positions(value, b.size);
  bloom_exists_i := 0;
  while bloom_exists_i < Length(bloom_exists_pos) do begin
  bloom_exists_idx := (b.size - 1) - bloom_exists_pos[bloom_exists_i];
  if b.bits[bloom_exists_idx] <> 1 then begin
  exit(false);
end;
  bloom_exists_i := bloom_exists_i + 1;
end;
  exit(true);
end;
function bitstring(b: Bloom): string;
var
  bitstring_res: string;
  bitstring_i: integer;
begin
  bitstring_res := '';
  bitstring_i := 0;
  while bitstring_i < b.size do begin
  bitstring_res := bitstring_res + IntToStr(b.bits[bitstring_i]);
  bitstring_i := bitstring_i + 1;
end;
  exit(bitstring_res);
end;
function format_hash(b: Bloom; value: string): string;
var
  format_hash_pos: IntArray;
  format_hash_bits: array of integer;
  format_hash_i: integer;
  format_hash_idx: integer;
  format_hash_res: string;
begin
  format_hash_pos := hash_positions(value, b.size);
  format_hash_bits := [];
  format_hash_i := 0;
  while format_hash_i < b.size do begin
  format_hash_bits := concat(format_hash_bits, IntArray([0]));
  format_hash_i := format_hash_i + 1;
end;
  format_hash_i := 0;
  while format_hash_i < Length(format_hash_pos) do begin
  format_hash_idx := (b.size - 1) - format_hash_pos[format_hash_i];
  format_hash_bits[format_hash_idx] := 1;
  format_hash_i := format_hash_i + 1;
end;
  format_hash_res := '';
  format_hash_i := 0;
  while format_hash_i < b.size do begin
  format_hash_res := format_hash_res + IntToStr(format_hash_bits[format_hash_i]);
  format_hash_i := format_hash_i + 1;
end;
  exit(format_hash_res);
end;
function estimated_error_rate(b: Bloom): real;
var
  estimated_error_rate_ones: integer;
  estimated_error_rate_i: integer;
  estimated_error_rate_frac: real;
begin
  estimated_error_rate_ones := 0;
  estimated_error_rate_i := 0;
  while estimated_error_rate_i < b.size do begin
  if b.bits[estimated_error_rate_i] = 1 then begin
  estimated_error_rate_ones := estimated_error_rate_ones + 1;
end;
  estimated_error_rate_i := estimated_error_rate_i + 1;
end;
  estimated_error_rate_frac := Double(estimated_error_rate_ones) / Double(b.size);
  exit(estimated_error_rate_frac * estimated_error_rate_frac);
end;
function any_in(b: Bloom; items: StrArray): boolean;
var
  any_in_i: integer;
begin
  any_in_i := 0;
  while any_in_i < Length(items) do begin
  if bloom_exists(b, items[any_in_i]) then begin
  exit(true);
end;
  any_in_i := any_in_i + 1;
end;
  exit(false);
end;
procedure main();
var
  main_bloom_var: Bloom;
  main_not_present: array of string;
  main_i: integer;
  main_film: string;
begin
  main_bloom_var := new_bloom(8);
  writeln(bitstring(main_bloom_var));
  writeln(Ord(bloom_exists(main_bloom_var, 'Titanic')));
  main_bloom_var := bloom_add(main_bloom_var, 'Titanic');
  writeln(bitstring(main_bloom_var));
  writeln(Ord(bloom_exists(main_bloom_var, 'Titanic')));
  main_bloom_var := bloom_add(main_bloom_var, 'Avatar');
  writeln(Ord(bloom_exists(main_bloom_var, 'Avatar')));
  writeln(format_hash(main_bloom_var, 'Avatar'));
  writeln(bitstring(main_bloom_var));
  main_not_present := ['The Godfather', 'Interstellar', 'Parasite', 'Pulp Fiction'];
  main_i := 0;
  while main_i < Length(main_not_present) do begin
  main_film := main_not_present[main_i];
  writeln((main_film + ':') + format_hash(main_bloom_var, main_film));
  main_i := main_i + 1;
end;
  writeln(Ord(any_in(main_bloom_var, main_not_present)));
  writeln(Ord(bloom_exists(main_bloom_var, 'Ratatouille')));
  writeln(format_hash(main_bloom_var, 'Ratatouille'));
  writeln(FloatToStr(estimated_error_rate(main_bloom_var)));
  main_bloom_var := bloom_add(main_bloom_var, 'The Godfather');
  writeln(FloatToStr(estimated_error_rate(main_bloom_var)));
  writeln(bitstring(main_bloom_var));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ascii := ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
