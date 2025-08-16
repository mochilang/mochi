{$mode objfpc}{$modeswitch nestedprocvars}
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
  number_2: integer;
  lower: integer;
  to_guess: integer;
  number_1: integer;
  higher: integer;
function get_avg(number_1: integer; number_2: integer): integer; forward;
function guess_the_number(lower: integer; higher: integer; to_guess: integer): IntArray; forward;
function get_avg(number_1: integer; number_2: integer): integer;
begin
  exit((number_1 + number_2) div 2);
end;
function guess_the_number(lower: integer; higher: integer; to_guess: integer): IntArray;
var
  guess_the_number_last_lowest: integer;
  guess_the_number_last_highest: integer;
  guess_the_number_last_numbers: array of integer;
  guess_the_number_number: integer;
  guess_the_number_resp: string;
  function answer(answer_guess_the_number_number: integer): string;
begin
  if guess_the_number_number > to_guess then begin
  exit('high');
end else begin
  if guess_the_number_number < to_guess then begin
  exit('low');
end else begin
  exit('same');
end;
end;
end;
begin
  if lower > higher then begin
  panic('argument value for lower and higher must be(lower > higher)');
end;
  if not ((lower < to_guess) and (to_guess < higher)) then begin
  panic('guess value must be within the range of lower and higher value');
end;
  writeln('started...');
  guess_the_number_last_lowest := lower;
  guess_the_number_last_highest := higher;
  guess_the_number_last_numbers := [];
  while true do begin
  guess_the_number_number := get_avg(guess_the_number_last_lowest, guess_the_number_last_highest);
  guess_the_number_last_numbers := concat(guess_the_number_last_numbers, IntArray([guess_the_number_number]));
  guess_the_number_resp := answer(guess_the_number_number);
  if guess_the_number_resp = 'low' then begin
  guess_the_number_last_lowest := guess_the_number_number;
end else begin
  if guess_the_number_resp = 'high' then begin
  guess_the_number_last_highest := guess_the_number_number;
end else begin
  break;
end;
end;
end;
  writeln('guess the number : ' + IntToStr(guess_the_number_last_numbers[Length(guess_the_number_last_numbers) - 1]));
  writeln('details : ' + list_int_to_str(guess_the_number_last_numbers));
  exit(guess_the_number_last_numbers);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  guess_the_number(10, 1000, 17);
  guess_the_number(-10000, 10000, 7);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
