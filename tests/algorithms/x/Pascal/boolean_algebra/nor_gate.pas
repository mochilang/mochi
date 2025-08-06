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
  s: string;
  width: integer;
  i: integer;
  j: integer;
  input_2: integer;
  input_1: integer;
function nor_gate(input_1: integer; input_2: integer): integer; forward;
function center(s: string; width: integer): string; forward;
function make_table_row(i: integer; j: integer): string; forward;
function truth_table(): string; forward;
function nor_gate(input_1: integer; input_2: integer): integer;
begin
  if (input_1 = 0) and (input_2 = 0) then begin
  exit(1);
end;
  exit(0);
end;
function center(s: string; width: integer): string;
var
  center_total: integer;
  center_left: integer;
  center_right: integer;
  center_res: string;
  center_i: integer;
  center_j: integer;
begin
  center_total := width - Length(s);
  if center_total <= 0 then begin
  exit(s);
end;
  center_left := center_total div 2;
  center_right := center_total - center_left;
  center_res := s;
  center_i := 0;
  while center_i < center_left do begin
  center_res := ' ' + center_res;
  center_i := center_i + 1;
end;
  center_j := 0;
  while center_j < center_right do begin
  center_res := center_res + ' ';
  center_j := center_j + 1;
end;
  exit(center_res);
end;
function make_table_row(i: integer; j: integer): string;
var
  make_table_row_output: integer;
begin
  make_table_row_output := nor_gate(i, j);
  exit(((((('| ' + center(IntToStr(i), 8)) + ' | ') + center(IntToStr(j), 8)) + ' | ') + center(IntToStr(make_table_row_output), 8)) + ' |');
end;
function truth_table(): string;
begin
  exit(((((((('Truth Table of NOR Gate:' + #10 + '' + '| Input 1 | Input 2 | Output  |' + #10 + '') + make_table_row(0, 0)) + '' + #10 + '') + make_table_row(0, 1)) + '' + #10 + '') + make_table_row(1, 0)) + '' + #10 + '') + make_table_row(1, 1));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(nor_gate(0, 0));
  writeln(nor_gate(0, 1));
  writeln(nor_gate(1, 0));
  writeln(nor_gate(1, 1));
  writeln(truth_table());
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
