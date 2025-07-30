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
procedure each(xs: IntArray; f: ); forward;
function Map(xs: IntArray; f: ): IntArray; forward;
procedure anon2(i: integer); forward;
function anon3(i: integer): integer; forward;
procedure main(); forward;
procedure each(xs: IntArray; f: );
var
  each_x: integer;
begin
  for each_x in xs do begin
  f(each_x);
end;
end;
function Map(xs: IntArray; f: ): IntArray;
var
  Map_r: array of integer;
  Map_x: integer;
begin
  Map_r := [];
  for Map_x in xs do begin
  Map_r := concat(Map_r, [f(Map_x)]);
end;
  exit(Map_r);
end;
procedure anon2(i: integer);
begin
  exit(print(IntToStr(i * i)));
end;
function anon3(i: integer): integer;
begin
  exit(i * i);
end;
procedure main();
var
  main_s: array of integer;
begin
  main_s := [1, 2, 3, 4, 5];
  each(main_s, anon2);
  writeln(list_int_to_str(Map(main_s, anon3)));
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
