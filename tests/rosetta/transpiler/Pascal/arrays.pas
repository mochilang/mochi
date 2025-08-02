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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: array of integer;
  s: array of integer;
  cap_s: integer;
  i: integer;
function listStr(xs: IntArray): string; forward;
function listStr(xs: IntArray): string;
var
  listStr_s: string;
  listStr_i: integer;
begin
  listStr_s := '[';
  listStr_i := 0;
  while listStr_i < Length(xs) do begin
  listStr_s := listStr_s + IntToStr(xs[listStr_i]);
  if (listStr_i + 1) < Length(xs) then begin
  listStr_s := listStr_s + ' ';
end;
  listStr_i := listStr_i + 1;
end;
  listStr_s := listStr_s + ']';
  exit(listStr_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  a := [0, 0, 0, 0, 0];
  writeln('len(a) = ' + IntToStr(Length(a)));
  writeln('a = ' + listStr(a));
  a[0] := 3;
  writeln('a = ' + listStr(a));
  writeln('a[0] = ' + IntToStr(a[0]));
  s := copy(a, 0, (4 - (0)));
  cap_s := 5;
  writeln('s = ' + listStr(s));
  writeln((('len(s) = ' + IntToStr(Length(s))) + '  cap(s) = ') + IntToStr(cap_s));
  s := copy(a, 0, (5 - (0)));
  writeln('s = ' + listStr(s));
  a[0] := 22;
  s[0] := 22;
  writeln('a = ' + listStr(a));
  writeln('s = ' + listStr(s));
  s := concat(s, [4]);
  s := concat(s, [5]);
  s := concat(s, [6]);
  cap_s := 10;
  writeln('s = ' + listStr(s));
  writeln((('len(s) = ' + IntToStr(Length(s))) + '  cap(s) = ') + IntToStr(cap_s));
  a[4] := -1;
  writeln('a = ' + listStr(a));
  writeln('s = ' + listStr(s));
  s := [];
  for i := 0 to (8 - 1) do begin
  s := concat(s, [0]);
end;
  cap_s := 8;
  writeln('s = ' + listStr(s));
  writeln((('len(s) = ' + IntToStr(Length(s))) + '  cap(s) = ') + IntToStr(cap_s));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
