{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  input_1: integer;
  input_2: integer;
  inputs: IntArray;
function and_gate(input_1: integer; input_2: integer): integer; forward;
function n_input_and_gate(inputs: IntArray): integer; forward;
function and_gate(input_1: integer; input_2: integer): integer;
begin
  if (input_1 <> 0) and (input_2 <> 0) then begin
  exit(1);
end;
  exit(0);
end;
function n_input_and_gate(inputs: IntArray): integer;
var
  n_input_and_gate_i: integer;
begin
  n_input_and_gate_i := 0;
  while n_input_and_gate_i < Length(inputs) do begin
  if inputs[n_input_and_gate_i] = 0 then begin
  exit(0);
end;
  n_input_and_gate_i := n_input_and_gate_i + 1;
end;
  exit(1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(and_gate(0, 0));
  writeln(and_gate(0, 1));
  writeln(and_gate(1, 0));
  writeln(and_gate(1, 1));
  writeln(n_input_and_gate([1, 0, 1, 1, 0]));
  writeln(n_input_and_gate([1, 1, 1, 1, 1]));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
