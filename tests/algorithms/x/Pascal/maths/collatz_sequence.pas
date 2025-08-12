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
  n: integer;
function collatz_sequence(n: integer): IntArray; forward;
procedure main(); forward;
function collatz_sequence(n: integer): IntArray;
var
  collatz_sequence_seq: array of integer;
  collatz_sequence_current: integer;
begin
  if n < 1 then begin
  panic('Sequence only defined for positive integers');
end;
  collatz_sequence_seq := [n];
  collatz_sequence_current := n;
  while collatz_sequence_current <> 1 do begin
  if (collatz_sequence_current mod 2) = 0 then begin
  collatz_sequence_current := collatz_sequence_current div 2;
end else begin
  collatz_sequence_current := (3 * collatz_sequence_current) + 1;
end;
  collatz_sequence_seq := concat(collatz_sequence_seq, IntArray([collatz_sequence_current]));
end;
  exit(collatz_sequence_seq);
end;
procedure main();
var
  main_n: integer;
  main_seq: IntArray;
begin
  main_n := 11;
  main_seq := collatz_sequence(main_n);
  writeln(list_int_to_str(main_seq));
  writeln(((('Collatz sequence from ' + IntToStr(main_n)) + ' took ') + IntToStr(Length(main_seq))) + ' steps.');
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
