{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  arr1: array of integer;
  counts1: specialize TFPGMap<integer, integer>;
  keys1: array of integer;
  i: integer;
  v: integer;
  max1: integer;
  k: integer;
  c: integer;
  c_idx: integer;
  modes1: array of integer;
  arr2: array of integer;
  counts2: specialize TFPGMap<integer, integer>;
  keys2: array of integer;
  max2: integer;
  modes2: array of integer;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr1 := [2, 7, 1, 8, 2];
  counts1 := specialize TFPGMap<integer, integer>.Create();
  keys1 := [];
  i := 0;
  while i < Length(arr1) do begin
  v := arr1[i];
  if counts1.IndexOf(v) <> -1 then begin
  counts1.AddOrSetData(v, counts1[v] + 1);
end else begin
  counts1.AddOrSetData(v, 1);
  keys1 := concat(keys1, [v]);
end;
  i := i + 1;
end;
  max1 := 0;
  i := 0;
  while i < Length(keys1) do begin
  k := keys1[i];
  c_idx := counts1.IndexOf(k);
  if c_idx <> -1 then begin
  c := counts1.Data[c_idx];
end else begin
  c := 0;
end;
  if c > max1 then begin
  max1 := c;
end;
  i := i + 1;
end;
  modes1 := [];
  i := 0;
  while i < Length(keys1) do begin
  k := keys1[i];
  if counts1[k] = max1 then begin
  modes1 := concat(modes1, [k]);
end;
  i := i + 1;
end;
  writeln(list_int_to_str(modes1));
  arr2 := [2, 7, 1, 8, 2, 8];
  counts2 := specialize TFPGMap<integer, integer>.Create();
  keys2 := [];
  i := 0;
  while i < Length(arr2) do begin
  v := arr2[i];
  if counts2.IndexOf(v) <> -1 then begin
  counts2.AddOrSetData(v, counts2[v] + 1);
end else begin
  counts2.AddOrSetData(v, 1);
  keys2 := concat(keys2, [v]);
end;
  i := i + 1;
end;
  max2 := 0;
  i := 0;
  while i < Length(keys2) do begin
  k := keys2[i];
  c_idx := counts2.IndexOf(k);
  if c_idx <> -1 then begin
  c := counts2.Data[c_idx];
end else begin
  c := 0;
end;
  if c > max2 then begin
  max2 := c;
end;
  i := i + 1;
end;
  modes2 := [];
  i := 0;
  while i < Length(keys2) do begin
  k := keys2[i];
  if counts2[k] = max2 then begin
  modes2 := concat(modes2, [k]);
end;
  i := i + 1;
end;
  writeln(list_int_to_str(modes2));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
