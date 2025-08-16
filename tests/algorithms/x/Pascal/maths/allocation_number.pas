{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  number_of_bytes: integer;
  partitions: integer;
function allocation_num(number_of_bytes: integer; partitions: integer): StrArray; forward;
function allocation_num(number_of_bytes: integer; partitions: integer): StrArray;
var
  allocation_num_bytes_per_partition: integer;
  allocation_num_allocation_list: array of string;
  allocation_num_i: integer;
  allocation_num_start_bytes: integer;
  allocation_num_end_bytes: integer;
begin
  if partitions <= 0 then begin
  panic('partitions must be a positive number!');
end;
  if partitions > number_of_bytes then begin
  panic('partitions can not > number_of_bytes!');
end;
  allocation_num_bytes_per_partition := number_of_bytes div partitions;
  allocation_num_allocation_list := [];
  allocation_num_i := 0;
  while allocation_num_i < partitions do begin
  allocation_num_start_bytes := (allocation_num_i * allocation_num_bytes_per_partition) + 1;
  if allocation_num_i = (partitions - 1) then begin
  allocation_num_end_bytes := number_of_bytes;
end else begin
  allocation_num_end_bytes := (allocation_num_i + 1) * allocation_num_bytes_per_partition;
end;
  allocation_num_allocation_list := concat(allocation_num_allocation_list, StrArray([(IntToStr(allocation_num_start_bytes) + '-') + IntToStr(allocation_num_end_bytes)]));
  allocation_num_i := allocation_num_i + 1;
end;
  exit(allocation_num_allocation_list);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_to_str(allocation_num(16647, 4)));
  writeln(list_to_str(allocation_num(50000, 5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
