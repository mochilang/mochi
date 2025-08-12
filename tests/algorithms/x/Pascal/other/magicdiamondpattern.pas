{$mode objfpc}{$modeswitch nestedprocvars}
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
  n: integer;
function floyd(n: integer): string; forward;
function reverse_floyd(n: integer): string; forward;
function pretty_print(n: integer): string; forward;
procedure main(); forward;
function floyd(n: integer): string;
var
  floyd_result_: string;
  floyd_i: integer;
  floyd_j: integer;
  floyd_k: integer;
begin
  floyd_result_ := '';
  floyd_i := 0;
  while floyd_i < n do begin
  floyd_j := 0;
  while floyd_j < ((n - floyd_i) - 1) do begin
  floyd_result_ := floyd_result_ + ' ';
  floyd_j := floyd_j + 1;
end;
  floyd_k := 0;
  while floyd_k < (floyd_i + 1) do begin
  floyd_result_ := floyd_result_ + '* ';
  floyd_k := floyd_k + 1;
end;
  floyd_result_ := floyd_result_ + '' + #10 + '';
  floyd_i := floyd_i + 1;
end;
  exit(floyd_result_);
end;
function reverse_floyd(n: integer): string;
var
  reverse_floyd_result_: string;
  reverse_floyd_i: integer;
  reverse_floyd_j: integer;
  reverse_floyd_k: integer;
begin
  reverse_floyd_result_ := '';
  reverse_floyd_i := n;
  while reverse_floyd_i > 0 do begin
  reverse_floyd_j := reverse_floyd_i;
  while reverse_floyd_j > 0 do begin
  reverse_floyd_result_ := reverse_floyd_result_ + '* ';
  reverse_floyd_j := reverse_floyd_j - 1;
end;
  reverse_floyd_result_ := reverse_floyd_result_ + '' + #10 + '';
  reverse_floyd_k := (n - reverse_floyd_i) + 1;
  while reverse_floyd_k > 0 do begin
  reverse_floyd_result_ := reverse_floyd_result_ + ' ';
  reverse_floyd_k := reverse_floyd_k - 1;
end;
  reverse_floyd_i := reverse_floyd_i - 1;
end;
  exit(reverse_floyd_result_);
end;
function pretty_print(n: integer): string;
var
  pretty_print_upper_half: string;
  pretty_print_lower_half: string;
begin
  if n <= 0 then begin
  exit('       ...       ....        nothing printing :(');
end;
  pretty_print_upper_half := floyd(n);
  pretty_print_lower_half := reverse_floyd(n);
  exit(pretty_print_upper_half + pretty_print_lower_half);
end;
procedure main();
begin
  writeln(pretty_print(3));
  writeln(pretty_print(0));
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
