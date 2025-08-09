{$mode objfpc}
program Main;
uses SysUtils;
type Fibonacci = record
  sequence: array of integer;
end;
type FibGetResult = record
  fib: Fibonacci;
  values: array of integer;
end;
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
  index: integer;
  f: Fibonacci;
function makeFibGetResult(fib: Fibonacci; values: IntArray): FibGetResult; forward;
function makeFibonacci(sequence: IntArray): Fibonacci; forward;
function create_fibonacci(): Fibonacci; forward;
function fib_get(f: Fibonacci; index: integer): FibGetResult; forward;
procedure main(); forward;
function makeFibGetResult(fib: Fibonacci; values: IntArray): FibGetResult;
begin
  Result.fib := fib;
  Result.values := values;
end;
function makeFibonacci(sequence: IntArray): Fibonacci;
begin
  Result.sequence := sequence;
end;
function create_fibonacci(): Fibonacci;
begin
  exit(makeFibonacci([0, 1]));
end;
function fib_get(f: Fibonacci; index: integer): FibGetResult;
var
  fib_get_seq: array of integer;
  fib_get_next: integer;
  fib_get_result_: array of integer;
  fib_get_i: integer;
begin
  fib_get_seq := f.sequence;
  while Length(fib_get_seq) < index do begin
  fib_get_next := fib_get_seq[Length(fib_get_seq) - 1] + fib_get_seq[Length(fib_get_seq) - 2];
  fib_get_seq := concat(fib_get_seq, IntArray([fib_get_next]));
end;
  f.sequence := fib_get_seq;
  fib_get_result_ := [];
  fib_get_i := 0;
  while fib_get_i < index do begin
  fib_get_result_ := concat(fib_get_result_, IntArray([fib_get_seq[fib_get_i]]));
  fib_get_i := fib_get_i + 1;
end;
  exit(makeFibGetResult(f, fib_get_result_));
end;
procedure main();
var
  main_fib: Fibonacci;
  main_res: FibGetResult;
begin
  main_fib := create_fibonacci();
  main_res := fib_get(main_fib, 10);
  main_fib := main_res.fib;
  writeln(list_int_to_str(main_res.values));
  main_res := fib_get(main_fib, 5);
  main_fib := main_res.fib;
  writeln(list_int_to_str(main_res.values));
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
end.
