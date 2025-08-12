{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type BoolArray = array of boolean;
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
  n: integer;
function totient(n: integer): IntArray; forward;
procedure test_totient(); forward;
procedure main(); forward;
function totient(n: integer): IntArray;
var
  totient_is_prime: array of boolean;
  totient_totients: array of integer;
  totient_primes: array of integer;
  totient_i: integer;
  totient_j: integer;
  totient_p: integer;
begin
  totient_is_prime := [];
  totient_totients := [];
  totient_primes := [];
  totient_i := 0;
  while totient_i <= n do begin
  totient_is_prime := concat(totient_is_prime, [true]);
  totient_totients := concat(totient_totients, IntArray([totient_i - 1]));
  totient_i := totient_i + 1;
end;
  totient_i := 2;
  while totient_i <= n do begin
  if totient_is_prime[totient_i] then begin
  totient_primes := concat(totient_primes, IntArray([totient_i]));
end;
  totient_j := 0;
  while totient_j < Length(totient_primes) do begin
  totient_p := totient_primes[totient_j];
  if (totient_i * totient_p) >= n then begin
  break;
end;
  totient_is_prime[totient_i * totient_p] := false;
  if (totient_i mod totient_p) = 0 then begin
  totient_totients[totient_i * totient_p] := totient_totients[totient_i] * totient_p;
  break;
end;
  totient_totients[totient_i * totient_p] := totient_totients[totient_i] * (totient_p - 1);
  totient_j := totient_j + 1;
end;
  totient_i := totient_i + 1;
end;
  exit(totient_totients);
end;
procedure test_totient();
var
  test_totient_expected: array of integer;
  test_totient_res: IntArray;
  test_totient_idx: integer;
begin
  test_totient_expected := [-1, 0, 1, 2, 2, 4, 2, 6, 4, 6, 9];
  test_totient_res := totient(10);
  test_totient_idx := 0;
  while test_totient_idx < Length(test_totient_expected) do begin
  if test_totient_res[test_totient_idx] <> test_totient_expected[test_totient_idx] then begin
  panic('totient mismatch at ' + IntToStr(test_totient_idx));
end;
  test_totient_idx := test_totient_idx + 1;
end;
end;
procedure main();
var
  main_n: integer;
  main_res: IntArray;
  main_i: integer;
begin
  test_totient();
  main_n := 10;
  main_res := totient(main_n);
  main_i := 1;
  while main_i < main_n do begin
  writeln(((IntToStr(main_i) + ' has ') + IntToStr(main_res[main_i])) + ' relative primes.');
  main_i := main_i + 1;
end;
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
