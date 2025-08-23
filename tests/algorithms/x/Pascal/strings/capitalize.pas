{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils;
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
  lowercase: string;
  uppercase: string;
function index_of(index_of_s: string; index_of_c: string): integer; forward;
function capitalize(capitalize_sentence: string): string; forward;
function index_of(index_of_s: string; index_of_c: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(index_of_s) do begin
  if copy(index_of_s, index_of_i+1, (index_of_i + 1 - (index_of_i))) = index_of_c then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function capitalize(capitalize_sentence: string): string;
var
  capitalize_first: string;
  capitalize_idx: integer;
  capitalize_capital: string;
begin
  if Length(capitalize_sentence) = 0 then begin
  exit('');
end;
  capitalize_first := copy(capitalize_sentence, 1, 1);
  capitalize_idx := index_of(lowercase, capitalize_first);
  if capitalize_idx >= 0 then begin
  capitalize_capital := copy(uppercase, capitalize_idx+1, (capitalize_idx + 1 - (capitalize_idx)));
end else begin
  capitalize_capital := capitalize_first;
end;
  exit(capitalize_capital + copy(capitalize_sentence, 2, (Length(capitalize_sentence) - (1))));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lowercase := 'abcdefghijklmnopqrstuvwxyz';
  uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  writeln(capitalize('hello world'));
  writeln(capitalize('123 hello world'));
  writeln(capitalize(' hello world'));
  writeln(capitalize('a'));
  writeln(capitalize(''));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
