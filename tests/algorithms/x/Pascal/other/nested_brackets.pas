{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type StrArray = array of string;
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
  OPEN_TO_CLOSED: specialize TFPGMap<string, string>;
  xs: StrArray;
  s: string;
function Map1(): specialize TFPGMap<string, string>; forward;
function slice_without_last(xs: StrArray): StrArray; forward;
function is_balanced(s: string): boolean; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, string>;
begin
  Result := specialize TFPGMap<string, string>.Create();
  Result.AddOrSetData('(', ')');
  Result.AddOrSetData('[', ']');
  Result.AddOrSetData('{', '}');
end;
function slice_without_last(xs: StrArray): StrArray;
var
  slice_without_last_res: array of string;
  slice_without_last_i: integer;
begin
  slice_without_last_res := [];
  slice_without_last_i := 0;
  while slice_without_last_i < (Length(xs) - 1) do begin
  slice_without_last_res := concat(slice_without_last_res, StrArray([xs[slice_without_last_i]]));
  slice_without_last_i := slice_without_last_i + 1;
end;
  exit(slice_without_last_res);
end;
function is_balanced(s: string): boolean;
var
  is_balanced_stack: array of string;
  is_balanced_i: integer;
  is_balanced_symbol: string;
  is_balanced_top: string;
begin
  is_balanced_stack := [];
  is_balanced_i := 0;
  while is_balanced_i < Length(s) do begin
  is_balanced_symbol := copy(s, is_balanced_i+1, (is_balanced_i + 1 - (is_balanced_i)));
  if OPEN_TO_CLOSED.IndexOf(is_balanced_symbol) <> -1 then begin
  is_balanced_stack := concat(is_balanced_stack, StrArray([is_balanced_symbol]));
end else begin
  if ((is_balanced_symbol = ')') or (is_balanced_symbol = ']')) or (is_balanced_symbol = '}') then begin
  if Length(is_balanced_stack) = 0 then begin
  exit(false);
end;
  is_balanced_top := is_balanced_stack[Length(is_balanced_stack) - 1];
  if OPEN_TO_CLOSED[is_balanced_top] <> is_balanced_symbol then begin
  exit(false);
end;
  is_balanced_stack := slice_without_last(is_balanced_stack);
end;
end;
  is_balanced_i := is_balanced_i + 1;
end;
  exit(Length(is_balanced_stack) = 0);
end;
procedure main();
begin
  writeln(Ord(is_balanced('')));
  writeln(Ord(is_balanced('()')));
  writeln(Ord(is_balanced('[]')));
  writeln(Ord(is_balanced('{}')));
  writeln(Ord(is_balanced('()[]{}')));
  writeln(Ord(is_balanced('(())')));
  writeln(Ord(is_balanced('[[')));
  writeln(Ord(is_balanced('([{}])')));
  writeln(Ord(is_balanced('(()[)]')));
  writeln(Ord(is_balanced('([)]')));
  writeln(Ord(is_balanced('[[()]]')));
  writeln(Ord(is_balanced('(()(()))')));
  writeln(Ord(is_balanced(']')));
  writeln(Ord(is_balanced('Life is a bowl of cherries.')));
  writeln(Ord(is_balanced('Life is a bowl of che{}ies.')));
  writeln(Ord(is_balanced('Life is a bowl of che}{ies.')));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  OPEN_TO_CLOSED := Map1();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
