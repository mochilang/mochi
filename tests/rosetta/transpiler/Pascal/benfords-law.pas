{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  s: string;
function floorf(x: real): real; forward;
function indexOf(s: string; ch: string): integer; forward;
function fmtF3(x: real): string; forward;
function padFloat3(x: real; width: integer): string; forward;
function fib1000(): RealArray; forward;
function leadingDigit(x: real): integer; forward;
procedure show(nums: RealArray; title: string); forward;
procedure main(); forward;
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
function fmtF3(x: real): string;
var
  fmtF3_y: real;
  fmtF3_dot: integer;
  fmtF3_decs: integer;
begin
  fmtF3_y := floorf((x * 1000) + 0.5) / 1000;
  s := FloatToStr(fmtF3_y);
  fmtF3_dot := indexOf(s, '.');
  if fmtF3_dot = (0 - 1) then begin
  s := s + '.000';
end else begin
  fmtF3_decs := (Length(s) - fmtF3_dot) - 1;
  if fmtF3_decs > 3 then begin
  s := copy(s, 0+1, (fmtF3_dot + 4 - (0)));
end else begin
  while fmtF3_decs < 3 do begin
  s := s + '0';
  fmtF3_decs := fmtF3_decs + 1;
end;
end;
end;
  exit(s);
end;
function padFloat3(x: real; width: integer): string;
begin
  s := fmtF3(x);
  while Length(s) < width do begin
  s := ' ' + s;
end;
  exit(s);
end;
function fib1000(): RealArray;
var
  fib1000_a: real;
  fib1000_b: real;
  fib1000_res: array of real;
  fib1000_i: integer;
  fib1000_t: real;
begin
  fib1000_a := 0;
  fib1000_b := 1;
  fib1000_res := [];
  fib1000_i := 0;
  while fib1000_i < 1000 do begin
  fib1000_res := concat(fib1000_res, [fib1000_b]);
  fib1000_t := fib1000_b;
  fib1000_b := fib1000_b + fib1000_a;
  fib1000_a := fib1000_t;
  fib1000_i := fib1000_i + 1;
end;
  exit(fib1000_res);
end;
function leadingDigit(x: real): integer;
begin
  if x < 0 then begin
  x := -x;
end;
  while x >= 10 do begin
  x := x / 10;
end;
  while (x > 0) and (x < 1) do begin
  x := x * 10;
end;
  exit(Trunc(x));
end;
procedure show(nums: RealArray; title: string);
var
  show_counts: array of integer;
  show_n: real;
  show_d: integer;
  show_preds: array of real;
  show_total: integer;
  show_i: integer;
  show_obs: real;
  show_line: string;
begin
  show_counts := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  for show_n in nums do begin
  show_d := leadingDigit(show_n);
  if (show_d >= 1) and (show_d <= 9) then begin
  show_counts[show_d - 1] := show_counts[show_d - 1] + 1;
end;
end;
  show_preds := [0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046];
  show_total := Length(nums);
  writeln(title);
  writeln('Digit  Observed  Predicted');
  show_i := 0;
  while show_i < 9 do begin
  show_obs := Double(show_counts[show_i]) / Double(show_total);
  show_line := (((('  ' + IntToStr(show_i + 1)) + '  ') + padFloat3(show_obs, 9)) + '  ') + padFloat3(show_preds[show_i], 8);
  writeln(show_line);
  show_i := show_i + 1;
end;
end;
procedure main();
begin
  show(fib1000(), 'First 1000 Fibonacci numbers');
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
