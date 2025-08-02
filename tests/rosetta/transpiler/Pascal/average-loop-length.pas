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
function absf(x: real): real; forward;
function floorf(x: real): real; forward;
function indexOf(s: string; ch: string): integer; forward;
function fmtF(x: real): string; forward;
function padInt(n: integer; width: integer): string; forward;
function padFloat(x: real; width: integer): string; forward;
function avgLen(n: integer): real; forward;
function ana(n: integer): real; forward;
procedure main(); forward;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function floorf(x: real): real;
var
  floorf_y: integer;
begin
  floorf_y := Trunc(x);
  exit(Double(floorf_y));
end;
function indexOf(s: string; ch: string): integer;
var
  indexOf_i: integer;
begin
  indexOf_i := 0;
  while indexOf_i < Length(s) do begin
  if copy(s, indexOf_i+1, (indexOf_i + 1 - (indexOf_i))) = ch then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(-1);
end;
function fmtF(x: real): string;
var
  fmtF_y: real;
  fmtF_s: string;
  fmtF_dot: integer;
  fmtF_decs: integer;
begin
  fmtF_y := floorf((x * 10000) + 0.5) / 10000;
  fmtF_s := FloatToStr(fmtF_y);
  fmtF_dot := indexOf(fmtF_s, '.');
  if fmtF_dot = (0 - 1) then begin
  fmtF_s := fmtF_s + '.0000';
end else begin
  fmtF_decs := (Length(fmtF_s) - fmtF_dot) - 1;
  if fmtF_decs > 4 then begin
  fmtF_s := copy(fmtF_s, 0+1, (fmtF_dot + 5 - (0)));
end else begin
  while fmtF_decs < 4 do begin
  fmtF_s := fmtF_s + '0';
  fmtF_decs := fmtF_decs + 1;
end;
end;
end;
  exit(fmtF_s);
end;
function padInt(n: integer; width: integer): string;
var
  padInt_s: string;
begin
  padInt_s := IntToStr(n);
  while Length(padInt_s) < width do begin
  padInt_s := ' ' + padInt_s;
end;
  exit(padInt_s);
end;
function padFloat(x: real; width: integer): string;
var
  padFloat_s: string;
begin
  padFloat_s := fmtF(x);
  while Length(padFloat_s) < width do begin
  padFloat_s := ' ' + padFloat_s;
end;
  exit(padFloat_s);
end;
function avgLen(n: integer): real;
var
  avgLen_tests: integer;
  avgLen_sum: integer;
  avgLen_seed: integer;
  avgLen_t: integer;
  avgLen_visited: array of boolean;
  avgLen_i: integer;
  avgLen_x: integer;
begin
  avgLen_tests := 10000;
  avgLen_sum := 0;
  avgLen_seed := 1;
  avgLen_t := 0;
  while avgLen_t < avgLen_tests do begin
  avgLen_visited := [];
  avgLen_i := 0;
  while avgLen_i < n do begin
  avgLen_visited := concat(avgLen_visited, [false]);
  avgLen_i := avgLen_i + 1;
end;
  avgLen_x := 0;
  while not avgLen_visited[avgLen_x] do begin
  avgLen_visited[avgLen_x] := true;
  avgLen_sum := avgLen_sum + 1;
  avgLen_seed := ((avgLen_seed * 1664525) + 1013904223) mod 2147483647;
  avgLen_x := avgLen_seed mod n;
end;
  avgLen_t := avgLen_t + 1;
end;
  exit(Double(avgLen_sum) / avgLen_tests);
end;
function ana(n: integer): real;
var
  ana_nn: real;
  ana_term: real;
  ana_sum: real;
  ana_i: real;
begin
  ana_nn := Double(n);
  ana_term := 1;
  ana_sum := 1;
  ana_i := ana_nn - 1;
  while ana_i >= 1 do begin
  ana_term := ana_term * (ana_i / ana_nn);
  ana_sum := ana_sum + ana_term;
  ana_i := ana_i - 1;
end;
  exit(ana_sum);
end;
procedure main();
var
  main_nmax: integer;
  main_n: integer;
  main_a: real;
  main_b: real;
  main_err: real;
  main_line: string;
begin
  main_nmax := 20;
  writeln(' N    average    analytical    (error)');
  writeln('===  =========  ============  =========');
  main_n := 1;
  while main_n <= main_nmax do begin
  main_a := avgLen(main_n);
  main_b := ana(main_n);
  main_err := (absf(main_a - main_b) / main_b) * 100;
  main_line := ((((((padInt(main_n, 3) + '  ') + padFloat(main_a, 9)) + '  ') + padFloat(main_b, 12)) + '  (') + padFloat(main_err, 6)) + '%)';
  writeln(main_line);
  main_n := main_n + 1;
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
