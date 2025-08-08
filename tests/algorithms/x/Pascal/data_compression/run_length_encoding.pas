{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  example1: string;
  encoded1: string;
  example2: string;
  encoded2: string;
  example3: string;
  encoded3: string;
  encoded: string;
  text: string;
function run_length_encode(text: string): string; forward;
function run_length_decode(encoded: string): string; forward;
function run_length_encode(text: string): string;
var
  run_length_encode_encoded: string;
  run_length_encode_count: integer;
  run_length_encode_i: integer;
begin
  if Length(text) = 0 then begin
  exit('');
end;
  run_length_encode_encoded := '';
  run_length_encode_count := 1;
  run_length_encode_i := 0;
  while run_length_encode_i < Length(text) do begin
  if ((run_length_encode_i + 1) < Length(text)) and (text[run_length_encode_i+1] = text[run_length_encode_i + 1+1]) then begin
  run_length_encode_count := run_length_encode_count + 1;
end else begin
  run_length_encode_encoded := (run_length_encode_encoded + text[run_length_encode_i+1]) + IntToStr(run_length_encode_count);
  run_length_encode_count := 1;
end;
  run_length_encode_i := run_length_encode_i + 1;
end;
  exit(run_length_encode_encoded);
end;
function run_length_decode(encoded: string): string;
var
  run_length_decode_res: string;
  run_length_decode_i: integer;
  run_length_decode_ch: string;
  run_length_decode_num_str: string;
  run_length_decode_count: integer;
  run_length_decode_j: integer;
begin
  run_length_decode_res := '';
  run_length_decode_i := 0;
  while run_length_decode_i < Length(encoded) do begin
  run_length_decode_ch := encoded[run_length_decode_i+1];
  run_length_decode_i := run_length_decode_i + 1;
  run_length_decode_num_str := '';
  while ((run_length_decode_i < Length(encoded)) and (encoded[run_length_decode_i+1] >= '0')) and (encoded[run_length_decode_i+1] <= '9') do begin
  run_length_decode_num_str := run_length_decode_num_str + encoded[run_length_decode_i+1];
  run_length_decode_i := run_length_decode_i + 1;
end;
  run_length_decode_count := StrToInt(run_length_decode_num_str);
  run_length_decode_j := 0;
  while run_length_decode_j < run_length_decode_count do begin
  run_length_decode_res := run_length_decode_res + run_length_decode_ch;
  run_length_decode_j := run_length_decode_j + 1;
end;
end;
  exit(run_length_decode_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := 'AAAABBBCCDAA';
  encoded1 := run_length_encode(example1);
  writeln(encoded1);
  writeln(run_length_decode(encoded1));
  example2 := 'A';
  encoded2 := run_length_encode(example2);
  writeln(encoded2);
  writeln(run_length_decode(encoded2));
  example3 := 'AAADDDDDDFFFCCCAAVVVV';
  encoded3 := run_length_encode(example3);
  writeln(encoded3);
  writeln(run_length_decode(encoded3));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
