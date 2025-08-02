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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
function prng(max: integer): integer; forward;
function gen(n: integer): string; forward;
procedure testBalanced(s: string); forward;
procedure main(); forward;
function prng(max: integer): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed mod max);
end;
function gen(n: integer): string;
var
  gen_arr: array of string;
  gen_i: integer;
  gen_j: integer;
  gen_k: integer;
  gen_tmp: string;
  gen_out: string;
  gen_ch: string;
begin
  gen_arr := [];
  gen_i := 0;
  while gen_i < n do begin
  gen_arr := concat(gen_arr, ['[']);
  gen_arr := concat(gen_arr, [']']);
  gen_i := gen_i + 1;
end;
  gen_j := Length(gen_arr) - 1;
  while gen_j > 0 do begin
  gen_k := prng(gen_j + 1);
  gen_tmp := gen_arr[gen_j];
  gen_arr[gen_j] := gen_arr[gen_k];
  gen_arr[gen_k] := gen_tmp;
  gen_j := gen_j - 1;
end;
  gen_out := '';
  for gen_ch in gen_arr do begin
  gen_out := gen_out + gen_ch;
end;
  exit(gen_out);
end;
procedure testBalanced(s: string);
var
  testBalanced_open: integer;
  testBalanced_i: integer;
  testBalanced_c: string;
begin
  testBalanced_open := 0;
  testBalanced_i := 0;
  while testBalanced_i < Length(s) do begin
  testBalanced_c := copy(s, testBalanced_i+1, (testBalanced_i + 1 - (testBalanced_i)));
  if testBalanced_c = '[' then begin
  testBalanced_open := testBalanced_open + 1;
end else begin
  if testBalanced_c = ']' then begin
  if testBalanced_open = 0 then begin
  writeln(s + ': not ok');
  exit();
end;
  testBalanced_open := testBalanced_open - 1;
end else begin
  writeln(s + ': not ok');
  exit();
end;
end;
  testBalanced_i := testBalanced_i + 1;
end;
  if testBalanced_open = 0 then begin
  writeln(s + ': ok');
end else begin
  writeln(s + ': not ok');
end;
end;
procedure main();
var
  main_i: integer;
begin
  main_i := 0;
  while main_i < 10 do begin
  testBalanced(gen(main_i));
  main_i := main_i + 1;
end;
  testBalanced('()');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
