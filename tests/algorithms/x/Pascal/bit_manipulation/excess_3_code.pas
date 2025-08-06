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
  number: integer;
function excess_3_code(number: integer): string; forward;
procedure main(); forward;
function excess_3_code(number: integer): string;
var
  excess_3_code_n: integer;
  excess_3_code_mapping: array of string;
  excess_3_code_res: string;
  excess_3_code_digit: integer;
begin
  excess_3_code_n := number;
  if excess_3_code_n < 0 then begin
  excess_3_code_n := 0;
end;
  excess_3_code_mapping := ['0011', '0100', '0101', '0110', '0111', '1000', '1001', '1010', '1011', '1100'];
  excess_3_code_res := '';
  if excess_3_code_n = 0 then begin
  excess_3_code_res := excess_3_code_mapping[0];
end else begin
  while excess_3_code_n > 0 do begin
  excess_3_code_digit := excess_3_code_n mod 10;
  excess_3_code_res := excess_3_code_mapping[excess_3_code_digit] + excess_3_code_res;
  excess_3_code_n := excess_3_code_n div 10;
end;
end;
  exit('0b' + excess_3_code_res);
end;
procedure main();
begin
  writeln(excess_3_code(0));
  writeln(excess_3_code(3));
  writeln(excess_3_code(2));
  writeln(excess_3_code(20));
  writeln(excess_3_code(120));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
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
