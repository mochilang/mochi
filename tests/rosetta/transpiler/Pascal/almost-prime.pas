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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function kPrime(n: integer; k: integer): boolean; forward;
function gen(k: integer; count: integer): IntArray; forward;
procedure main(); forward;
function kPrime(n: integer; k: integer): boolean;
var
  kPrime_nf: integer;
  kPrime_i: integer;
begin
  kPrime_nf := 0;
  kPrime_i := 2;
  while kPrime_i <= n do begin
  while (n mod kPrime_i) = 0 do begin
  if kPrime_nf = k then begin
  exit(false);
end;
  kPrime_nf := kPrime_nf + 1;
  n := n div kPrime_i;
end;
  kPrime_i := kPrime_i + 1;
end;
  exit(kPrime_nf = k);
end;
function gen(k: integer; count: integer): IntArray;
var
  gen_r: array of integer;
  gen_n: integer;
begin
  gen_r := [];
  gen_n := 2;
  while Length(gen_r) < count do begin
  if kPrime(gen_n, k) then begin
  gen_r := concat(gen_r, [gen_n]);
end;
  gen_n := gen_n + 1;
end;
  exit(gen_r);
end;
procedure main();
var
  main_k: integer;
begin
  main_k := 1;
  while main_k <= 5 do begin
  writeln((IntToStr(main_k) + ' ') + list_int_to_str(gen(main_k, 10)));
  main_k := main_k + 1;
end;
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
