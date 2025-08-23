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
function is_valid(is_valid_strand: string): boolean; forward;
function dna(dna_strand: string): string; forward;
function is_valid(is_valid_strand: string): boolean;
var
  is_valid_i: integer;
  is_valid_ch: string;
begin
  is_valid_i := 0;
  while is_valid_i < Length(is_valid_strand) do begin
  is_valid_ch := copy(is_valid_strand, is_valid_i+1, (is_valid_i + 1 - (is_valid_i)));
  if (((is_valid_ch <> 'A') and (is_valid_ch <> 'T')) and (is_valid_ch <> 'C')) and (is_valid_ch <> 'G') then begin
  exit(false);
end;
  is_valid_i := is_valid_i + 1;
end;
  exit(true);
end;
function dna(dna_strand: string): string;
var
  dna_result_: string;
  dna_i: integer;
  dna_ch: string;
begin
  if not is_valid(dna_strand) then begin
  writeln('ValueError: Invalid Strand');
  exit('');
end;
  dna_result_ := '';
  dna_i := 0;
  while dna_i < Length(dna_strand) do begin
  dna_ch := copy(dna_strand, dna_i+1, (dna_i + 1 - (dna_i)));
  if dna_ch = 'A' then begin
  dna_result_ := dna_result_ + 'T';
end else begin
  if dna_ch = 'T' then begin
  dna_result_ := dna_result_ + 'A';
end else begin
  if dna_ch = 'C' then begin
  dna_result_ := dna_result_ + 'G';
end else begin
  dna_result_ := dna_result_ + 'C';
end;
end;
end;
  dna_i := dna_i + 1;
end;
  exit(dna_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(dna('GCTA'));
  writeln(dna('ATGC'));
  writeln(dna('CTGA'));
  writeln(dna('GFGG'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
