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
  dna: string;
  le: integer;
  i: integer;
  k: integer;
  a: integer;
  c: integer;
  g: integer;
  t: integer;
  idx: integer;
  ch: string;
function padLeft(s: string; w: integer): string; forward;
function padLeft(s: string; w: integer): string;
var
  padLeft_res: string;
  padLeft_n: integer;
begin
  padLeft_res := '';
  padLeft_n := w - Length(s);
  while padLeft_n > 0 do begin
  padLeft_res := padLeft_res + ' ';
  padLeft_n := padLeft_n - 1;
end;
  exit(padLeft_res + s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  dna := ((((((((('' + 'CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG') + 'CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG') + 'AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT') + 'GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT') + 'CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG') + 'TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA') + 'TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT') + 'CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG') + 'TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC') + 'GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT';
  writeln('SEQUENCE:');
  le := Length(dna);
  i := 0;
  while i < le do begin
  k := i + 50;
  if k > le then begin
  k := le;
end;
  writeln((padLeft(IntToStr(i), 5) + ': ') + copy(dna, i+1, (k - (i))));
  i := i + 50;
end;
  a := 0;
  c := 0;
  g := 0;
  t := 0;
  idx := 0;
  while idx < le do begin
  ch := copy(dna, idx+1, (idx + 1 - (idx)));
  if ch = 'A' then begin
  a := a + 1;
end else begin
  if ch = 'C' then begin
  c := c + 1;
end else begin
  if ch = 'G' then begin
  g := g + 1;
end else begin
  if ch = 'T' then begin
  t := t + 1;
end;
end;
end;
end;
  idx := idx + 1;
end;
  writeln('');
  writeln('BASE COUNT:');
  writeln('    A: ' + padLeft(IntToStr(a), 3));
  writeln('    C: ' + padLeft(IntToStr(c), 3));
  writeln('    G: ' + padLeft(IntToStr(g), 3));
  writeln('    T: ' + padLeft(IntToStr(t), 3));
  writeln('    ------');
  writeln('    Σ: ' + IntToStr(le));
  writeln('    ======');
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
