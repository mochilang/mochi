{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  bf: specialize TFPGMap<integer, string>;
  input_string: string;
  key: integer;
function encrypt(input_string: string; key: integer): string; forward;
function decrypt(input_string: string; key: integer): string; forward;
function bruteforce(input_string: string): specialize TFPGMap<integer, string>; forward;
function encrypt(input_string: string; key: integer): string;
var
  encrypt_lowest: integer;
  encrypt_temp_grid: array of StrArray;
  encrypt_i: integer;
  encrypt_position: integer;
  encrypt_num: integer;
  encrypt_alt: integer;
  encrypt_row: array of string;
  encrypt_output: string;
  encrypt_j: integer;
begin
  if key <= 0 then begin
  panic('Height of grid can''t be 0 or negative');
end;
  if (key = 1) or (Length(input_string) <= key) then begin
  exit(input_string);
end;
  encrypt_lowest := key - 1;
  encrypt_temp_grid := [];
  encrypt_i := 0;
  while encrypt_i < key do begin
  encrypt_temp_grid := concat(encrypt_temp_grid, [StrArray([])]);
  encrypt_i := encrypt_i + 1;
end;
  encrypt_position := 0;
  while encrypt_position < Length(input_string) do begin
  encrypt_num := encrypt_position mod (encrypt_lowest * 2);
  encrypt_alt := (encrypt_lowest * 2) - encrypt_num;
  if encrypt_num > encrypt_alt then begin
  encrypt_num := encrypt_alt;
end;
  encrypt_row := encrypt_temp_grid[encrypt_num];
  encrypt_row := concat(encrypt_row, StrArray([copy(input_string, encrypt_position+1, (encrypt_position + 1 - (encrypt_position)))]));
  encrypt_temp_grid[encrypt_num] := encrypt_row;
  encrypt_position := encrypt_position + 1;
end;
  encrypt_output := '';
  encrypt_i := 0;
  while encrypt_i < key do begin
  encrypt_row := encrypt_temp_grid[encrypt_i];
  encrypt_j := 0;
  while encrypt_j < Length(encrypt_row) do begin
  encrypt_output := encrypt_output + encrypt_row[encrypt_j];
  encrypt_j := encrypt_j + 1;
end;
  encrypt_i := encrypt_i + 1;
end;
  exit(encrypt_output);
end;
function decrypt(input_string: string; key: integer): string;
var
  decrypt_lowest: integer;
  decrypt_counts: array of integer;
  decrypt_i: integer;
  decrypt_pos: integer;
  decrypt_num: integer;
  decrypt_alt: integer;
  decrypt_grid: array of StrArray;
  decrypt_counter: integer;
  decrypt_length: integer;
  decrypt_slice: string;
  decrypt_row: array of string;
  decrypt_j: integer;
  decrypt_indices: array of integer;
  decrypt_output: string;
begin
  if key <= 0 then begin
  panic('Height of grid can''t be 0 or negative');
end;
  if key = 1 then begin
  exit(input_string);
end;
  decrypt_lowest := key - 1;
  decrypt_counts := [];
  decrypt_i := 0;
  while decrypt_i < key do begin
  decrypt_counts := concat(decrypt_counts, IntArray([0]));
  decrypt_i := decrypt_i + 1;
end;
  decrypt_pos := 0;
  while decrypt_pos < Length(input_string) do begin
  decrypt_num := decrypt_pos mod (decrypt_lowest * 2);
  decrypt_alt := (decrypt_lowest * 2) - decrypt_num;
  if decrypt_num > decrypt_alt then begin
  decrypt_num := decrypt_alt;
end;
  decrypt_counts[decrypt_num] := decrypt_counts[decrypt_num] + 1;
  decrypt_pos := decrypt_pos + 1;
end;
  decrypt_grid := [];
  decrypt_counter := 0;
  decrypt_i := 0;
  while decrypt_i < key do begin
  decrypt_length := decrypt_counts[decrypt_i];
  decrypt_slice := copy(input_string, decrypt_counter+1, (decrypt_counter + decrypt_length - (decrypt_counter)));
  decrypt_row := [];
  decrypt_j := 0;
  while decrypt_j < Length(decrypt_slice) do begin
  decrypt_row := concat(decrypt_row, StrArray([decrypt_slice[decrypt_j+1]]));
  decrypt_j := decrypt_j + 1;
end;
  decrypt_grid := concat(decrypt_grid, [decrypt_row]);
  decrypt_counter := decrypt_counter + decrypt_length;
  decrypt_i := decrypt_i + 1;
end;
  decrypt_indices := [];
  decrypt_i := 0;
  while decrypt_i < key do begin
  decrypt_indices := concat(decrypt_indices, IntArray([0]));
  decrypt_i := decrypt_i + 1;
end;
  decrypt_output := '';
  decrypt_pos := 0;
  while decrypt_pos < Length(input_string) do begin
  decrypt_num := decrypt_pos mod (decrypt_lowest * 2);
  decrypt_alt := (decrypt_lowest * 2) - decrypt_num;
  if decrypt_num > decrypt_alt then begin
  decrypt_num := decrypt_alt;
end;
  decrypt_output := decrypt_output + decrypt_grid[decrypt_num][decrypt_indices[decrypt_num]];
  decrypt_indices[decrypt_num] := decrypt_indices[decrypt_num] + 1;
  decrypt_pos := decrypt_pos + 1;
end;
  exit(decrypt_output);
end;
function bruteforce(input_string: string): specialize TFPGMap<integer, string>;
var
  bruteforce_results: specialize TFPGMap<integer, string>;
  bruteforce_key_guess: integer;
begin
  bruteforce_results := specialize TFPGMap<integer, string>.Create();
  bruteforce_key_guess := 1;
  while bruteforce_key_guess < Length(input_string) do begin
  bruteforce_results.AddOrSetData(bruteforce_key_guess, Variant(decrypt(input_string, bruteforce_key_guess)));
  bruteforce_key_guess := bruteforce_key_guess + 1;
end;
  exit(bruteforce_results);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(encrypt('Hello World', 4));
  writeln(decrypt('HWe olordll', 4));
  bf := bruteforce('HWe olordll');
  writeln(bf[4]);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
