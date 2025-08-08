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
  nir: real;
  index: string;
  blue: real;
  red: real;
  redEdge: real;
  green: real;
function ndvi(red: real; nir: real): real; forward;
function bndvi(blue: real; nir: real): real; forward;
function gndvi(green: real; nir: real): real; forward;
function ndre(redEdge: real; nir: real): real; forward;
function ccci(red: real; redEdge: real; nir: real): real; forward;
function cvi(red: real; green: real; nir: real): real; forward;
function gli(red: real; green: real; blue: real): real; forward;
function dvi(red: real; nir: real): real; forward;
function calc(index: string; red: real; green: real; blue: real; redEdge: real; nir: real): real; forward;
procedure main(); forward;
function ndvi(red: real; nir: real): real;
begin
  exit((nir - red) / (nir + red));
end;
function bndvi(blue: real; nir: real): real;
begin
  exit((nir - blue) / (nir + blue));
end;
function gndvi(green: real; nir: real): real;
begin
  exit((nir - green) / (nir + green));
end;
function ndre(redEdge: real; nir: real): real;
begin
  exit((nir - redEdge) / (nir + redEdge));
end;
function ccci(red: real; redEdge: real; nir: real): real;
begin
  exit(ndre(redEdge, nir) / ndvi(red, nir));
end;
function cvi(red: real; green: real; nir: real): real;
begin
  exit((nir * red) / (green * green));
end;
function gli(red: real; green: real; blue: real): real;
begin
  exit((((2 * green) - red) - blue) / (((2 * green) + red) + blue));
end;
function dvi(red: real; nir: real): real;
begin
  exit(nir / red);
end;
function calc(index: string; red: real; green: real; blue: real; redEdge: real; nir: real): real;
begin
  if index = 'NDVI' then begin
  exit(ndvi(red, nir));
end;
  if index = 'BNDVI' then begin
  exit(bndvi(blue, nir));
end;
  if index = 'GNDVI' then begin
  exit(gndvi(green, nir));
end;
  if index = 'NDRE' then begin
  exit(ndre(redEdge, nir));
end;
  if index = 'CCCI' then begin
  exit(ccci(red, redEdge, nir));
end;
  if index = 'CVI' then begin
  exit(cvi(red, green, nir));
end;
  if index = 'GLI' then begin
  exit(gli(red, green, blue));
end;
  if index = 'DVI' then begin
  exit(dvi(red, nir));
end;
  exit(0);
end;
procedure main();
var
  main_red: real;
  main_green: real;
  main_blue: real;
  main_redEdge: real;
  main_nir: real;
begin
  main_red := 50;
  main_green := 30;
  main_blue := 10;
  main_redEdge := 40;
  main_nir := 100;
  writeln('NDVI=' + FloatToStr(ndvi(main_red, main_nir)));
  writeln('CCCI=' + FloatToStr(ccci(main_red, main_redEdge, main_nir)));
  writeln('CVI=' + FloatToStr(cvi(main_red, main_green, main_nir)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
