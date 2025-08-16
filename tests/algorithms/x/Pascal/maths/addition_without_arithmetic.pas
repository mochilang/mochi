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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  MAX: integer;
  HALF: integer;
  b: integer;
  a: integer;
  n: integer;
  num: integer;
function to_unsigned(n: integer): integer; forward;
function from_unsigned(n: integer): integer; forward;
function bit_and(a: integer; b: integer): integer; forward;
function bit_xor(a: integer; b: integer): integer; forward;
function lshift1(num: integer): integer; forward;
function add(a: integer; b: integer): integer; forward;
function to_unsigned(n: integer): integer;
begin
  if n < 0 then begin
  exit(MAX + n);
end;
  exit(n);
end;
function from_unsigned(n: integer): integer;
begin
  if n >= HALF then begin
  exit(n - MAX);
end;
  exit(n);
end;
function bit_and(a: integer; b: integer): integer;
var
  bit_and_x: integer;
  bit_and_y: integer;
  bit_and_res: integer;
  bit_and_bit: integer;
  bit_and_i: integer;
begin
  bit_and_x := a;
  bit_and_y := b;
  bit_and_res := 0;
  bit_and_bit := 1;
  bit_and_i := 0;
  while bit_and_i < 32 do begin
  if ((bit_and_x mod 2) = 1) and ((bit_and_y mod 2) = 1) then begin
  bit_and_res := bit_and_res + bit_and_bit;
end;
  bit_and_x := bit_and_x div 2;
  bit_and_y := bit_and_y div 2;
  bit_and_bit := bit_and_bit * 2;
  bit_and_i := bit_and_i + 1;
end;
  exit(bit_and_res);
end;
function bit_xor(a: integer; b: integer): integer;
var
  bit_xor_x: integer;
  bit_xor_y: integer;
  bit_xor_res: integer;
  bit_xor_bit: integer;
  bit_xor_i: integer;
  bit_xor_abit: integer;
  bit_xor_bbit: integer;
begin
  bit_xor_x := a;
  bit_xor_y := b;
  bit_xor_res := 0;
  bit_xor_bit := 1;
  bit_xor_i := 0;
  while bit_xor_i < 32 do begin
  bit_xor_abit := bit_xor_x mod 2;
  bit_xor_bbit := bit_xor_y mod 2;
  if ((bit_xor_abit + bit_xor_bbit) mod 2) = 1 then begin
  bit_xor_res := bit_xor_res + bit_xor_bit;
end;
  bit_xor_x := bit_xor_x div 2;
  bit_xor_y := bit_xor_y div 2;
  bit_xor_bit := bit_xor_bit * 2;
  bit_xor_i := bit_xor_i + 1;
end;
  exit(bit_xor_res);
end;
function lshift1(num: integer): integer;
begin
  exit((num * 2) mod MAX);
end;
function add(a: integer; b: integer): integer;
var
  add_first: integer;
  add_second: integer;
  add_carry: integer;
  add_result_: integer;
begin
  add_first := to_unsigned(a);
  add_second := to_unsigned(b);
  while add_second <> 0 do begin
  add_carry := bit_and(add_first, add_second);
  add_first := bit_xor(add_first, add_second);
  add_second := lshift1(add_carry);
end;
  add_result_ := from_unsigned(add_first);
  exit(add_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  MAX := 4294967296;
  HALF := 2147483648;
  writeln(IntToStr(add(3, 5)));
  writeln(IntToStr(add(13, 5)));
  writeln(IntToStr(add(-7, 2)));
  writeln(IntToStr(add(0, -7)));
  writeln(IntToStr(add(-321, 0)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
