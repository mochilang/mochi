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
  b: array of integer;
  c: array of integer;
  d: array of integer;
  i: integer;
  z: array of integer;
  sub: array of integer;
  f: array of integer;
  val: integer;
  rem: array of integer;
function char(n: integer): string; forward;
function fromBytes(bs: IntArray): string; forward;
function char(n: integer): string;
var
  char_letters: string;
  char_idx: integer;
begin
  char_letters := 'abcdefghijklmnopqrstuvwxyz';
  char_idx := n - 97;
  if (char_idx < 0) or (char_idx >= Length(char_letters)) then begin
  exit('?');
end;
  exit(copy(char_letters, char_idx+1, (char_idx + 1 - (char_idx))));
end;
function fromBytes(bs: IntArray): string;
var
  fromBytes_s: string;
  fromBytes_i: integer;
begin
  fromBytes_s := '';
  fromBytes_i := 0;
  while fromBytes_i < Length(bs) do begin
  fromBytes_s := fromBytes_s + char(bs[fromBytes_i]);
  fromBytes_i := fromBytes_i + 1;
end;
  exit(fromBytes_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  b := [98, 105, 110, 97, 114, 121];
  writeln(list_int_to_str(b));
  c := b;
  writeln(list_int_to_str(c));
  writeln(LowerCase(BoolToStr(b = c, true)));
  d := [];
  i := 0;
  while i < Length(b) do begin
  d := concat(d, [b[i]]);
  i := i + 1;
end;
  d[1] := 97;
  d[4] := 110;
  writeln(fromBytes(b));
  writeln(fromBytes(d));
  writeln(LowerCase(BoolToStr(Length(b) = 0, true)));
  z := concat(b, [122]);
  writeln(fromBytes(z));
  sub := copy(b, 1, (3 - (1)));
  writeln(fromBytes(sub));
  f := [];
  i := 0;
  while i < Length(d) do begin
  val := d[i];
  if val = 110 then begin
  f := concat(f, [109]);
end else begin
  f := concat(f, [val]);
end;
  i := i + 1;
end;
  writeln((fromBytes(d) + ' -> ') + fromBytes(f));
  rem := [];
  rem := concat(rem, [b[0]]);
  i := 3;
  while i < Length(b) do begin
  rem := concat(rem, [b[i]]);
  i := i + 1;
end;
  writeln(fromBytes(rem));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
