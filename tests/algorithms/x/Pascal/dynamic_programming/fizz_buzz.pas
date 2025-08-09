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
  iterations: integer;
  number: integer;
function fizz_buzz(number: integer; iterations: integer): string; forward;
function fizz_buzz(number: integer; iterations: integer): string;
var
  fizz_buzz_out: string;
  fizz_buzz_n: integer;
begin
  if number < 1 then begin
  panic('starting number must be an integer and be more than 0');
end;
  if iterations < 1 then begin
  panic('Iterations must be done more than 0 times to play FizzBuzz');
end;
  fizz_buzz_out := '';
  fizz_buzz_n := number;
  while fizz_buzz_n <= iterations do begin
  if (fizz_buzz_n mod 3) = 0 then begin
  fizz_buzz_out := fizz_buzz_out + 'Fizz';
end;
  if (fizz_buzz_n mod 5) = 0 then begin
  fizz_buzz_out := fizz_buzz_out + 'Buzz';
end;
  if ((fizz_buzz_n mod 3) <> 0) and ((fizz_buzz_n mod 5) <> 0) then begin
  fizz_buzz_out := fizz_buzz_out + IntToStr(fizz_buzz_n);
end;
  fizz_buzz_out := fizz_buzz_out + ' ';
  fizz_buzz_n := fizz_buzz_n + 1;
end;
  exit(fizz_buzz_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(fizz_buzz(1, 7));
  writeln(fizz_buzz(1, 15));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
