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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function poly(p: integer): string; forward;
function aks(n: integer): boolean; forward;
procedure main(); forward;
function poly(p: integer): string;
var
  poly_s: string;
  poly_coef: integer;
  poly_i: integer;
  poly_d: integer;
begin
  poly_s := '';
  poly_coef := 1;
  poly_i := p;
  if poly_coef <> 1 then begin
  poly_s := poly_s + IntToStr(poly_coef);
end;
  while poly_i > 0 do begin
  poly_s := poly_s + 'x';
  if poly_i <> 1 then begin
  poly_s := (poly_s + '^') + IntToStr(poly_i);
end;
  poly_coef := Trunc((poly_coef * poly_i) div ((p - poly_i) + 1));
  poly_d := poly_coef;
  if ((p - (poly_i - 1)) mod 2) = 1 then begin
  poly_d := -poly_d;
end;
  if poly_d < 0 then begin
  poly_s := (poly_s + ' - ') + IntToStr(-poly_d);
end else begin
  poly_s := (poly_s + ' + ') + IntToStr(poly_d);
end;
  poly_i := poly_i - 1;
end;
  if poly_s = '' then begin
  poly_s := '1';
end;
  exit(poly_s);
end;
function aks(n: integer): boolean;
var
  aks_c: integer;
  aks_i: integer;
begin
  if n < 2 then begin
  exit(false);
end;
  aks_c := n;
  aks_i := 1;
  while aks_i < n do begin
  if (aks_c mod n) <> 0 then begin
  exit(false);
end;
  aks_c := Trunc((aks_c * (n - aks_i)) div (aks_i + 1));
  aks_i := aks_i + 1;
end;
  exit(true);
end;
procedure main();
var
  main_p: integer;
  main_first: boolean;
  main_line: string;
begin
  main_p := 0;
  while main_p <= 7 do begin
  writeln((IntToStr(main_p) + ':  ') + poly(main_p));
  main_p := main_p + 1;
end;
  main_first := true;
  main_p := 2;
  main_line := '';
  while main_p < 50 do begin
  if aks(main_p) then begin
  if main_first then begin
  main_line := main_line + IntToStr(main_p);
  main_first := false;
end else begin
  main_line := (main_line + ' ') + IntToStr(main_p);
end;
end;
  main_p := main_p + 1;
end;
  writeln(main_line);
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
