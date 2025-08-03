{$mode objfpc}
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
  h: IntArray;
  t: integer;
function image(): IntArrayArray; forward;
function histogram(g: IntArrayArray; bins: integer): IntArray; forward;
function medianThreshold(h: IntArray): integer; forward;
function threshold(g: IntArrayArray; t: integer): IntArrayArray; forward;
procedure printImage(g: IntArrayArray); forward;
procedure main(); forward;
function image(): IntArrayArray;
begin
  exit([[0, 0, 10000], [65535, 65535, 65535], [65535, 65535, 65535]]);
end;
function histogram(g: IntArrayArray; bins: integer): IntArray;
var
  histogram_h: array of integer;
  histogram_i: integer;
  histogram_y: integer;
  histogram_row: array of integer;
  histogram_x: integer;
  histogram_p: integer;
  histogram_idx: integer;
begin
  if bins <= 0 then begin
  bins := Length(g[0]);
end;
  histogram_h := [];
  histogram_i := 0;
  while histogram_i < bins do begin
  histogram_h := concat(histogram_h, [0]);
  histogram_i := histogram_i + 1;
end;
  histogram_y := 0;
  while histogram_y < Length(g) do begin
  histogram_row := g[histogram_y];
  histogram_x := 0;
  while histogram_x < Length(histogram_row) do begin
  histogram_p := histogram_row[histogram_x];
  histogram_idx := Trunc((histogram_p * (bins - 1)) div 65535);
  histogram_h[histogram_idx] := histogram_h[histogram_idx] + 1;
  histogram_x := histogram_x + 1;
end;
  histogram_y := histogram_y + 1;
end;
  exit(histogram_h);
end;
function medianThreshold(h: IntArray): integer;
var
  medianThreshold_lb: integer;
  medianThreshold_ub: integer;
  medianThreshold_lSum: integer;
  medianThreshold_uSum: integer;
begin
  medianThreshold_lb := 0;
  medianThreshold_ub := Length(h) - 1;
  medianThreshold_lSum := 0;
  medianThreshold_uSum := 0;
  while medianThreshold_lb <= medianThreshold_ub do begin
  if (medianThreshold_lSum + h[medianThreshold_lb]) < (medianThreshold_uSum + h[medianThreshold_ub]) then begin
  medianThreshold_lSum := medianThreshold_lSum + h[medianThreshold_lb];
  medianThreshold_lb := medianThreshold_lb + 1;
end else begin
  medianThreshold_uSum := medianThreshold_uSum + h[medianThreshold_ub];
  medianThreshold_ub := medianThreshold_ub - 1;
end;
end;
  exit(Trunc((medianThreshold_ub * 65535) div Length(h)));
end;
function threshold(g: IntArrayArray; t: integer): IntArrayArray;
var
  threshold_out: array of IntArray;
  threshold_y: integer;
  threshold_row: array of integer;
  threshold_newRow: array of integer;
  threshold_x: integer;
begin
  threshold_out := [];
  threshold_y := 0;
  while threshold_y < Length(g) do begin
  threshold_row := g[threshold_y];
  threshold_newRow := [];
  threshold_x := 0;
  while threshold_x < Length(threshold_row) do begin
  if threshold_row[threshold_x] < t then begin
  threshold_newRow := concat(threshold_newRow, [0]);
end else begin
  threshold_newRow := concat(threshold_newRow, [65535]);
end;
  threshold_x := threshold_x + 1;
end;
  threshold_out := concat(threshold_out, [threshold_newRow]);
  threshold_y := threshold_y + 1;
end;
  exit(threshold_out);
end;
procedure printImage(g: IntArrayArray);
var
  printImage_y: integer;
  printImage_row: array of integer;
  printImage_line: string;
  printImage_x: integer;
begin
  printImage_y := 0;
  while printImage_y < Length(g) do begin
  printImage_row := g[printImage_y];
  printImage_line := '';
  printImage_x := 0;
  while printImage_x < Length(printImage_row) do begin
  if printImage_row[printImage_x] = 0 then begin
  printImage_line := printImage_line + '0';
end else begin
  printImage_line := printImage_line + '1';
end;
  printImage_x := printImage_x + 1;
end;
  writeln(printImage_line);
  printImage_y := printImage_y + 1;
end;
end;
procedure main();
var
  main_img: IntArrayArray;
  main_bw: IntArrayArray;
begin
  main_img := image();
  h := histogram(main_img, 0);
  writeln('Histogram: ' + list_int_to_str(h));
  t := medianThreshold(h);
  writeln('Threshold: ' + IntToStr(t));
  main_bw := threshold(main_img, t);
  printImage(main_bw);
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
