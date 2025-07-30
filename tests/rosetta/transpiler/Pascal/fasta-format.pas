{$mode objfpc}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  FASTA: string;
function splitLines(s: string): StrArray; forward;
function parseFasta(text: string): StrArray; forward;
procedure main(); forward;
function splitLines(s: string): StrArray;
var
  splitLines_lines: array of string;
  splitLines_start: integer;
  splitLines_i: integer;
begin
  splitLines_lines := [];
  splitLines_start := 0;
  splitLines_i := 0;
  while splitLines_i < Length(s) do begin
  if copy(s, splitLines_i+1, (splitLines_i + 1 - (splitLines_i))) = '' + #10 + '' then begin
  splitLines_lines := concat(splitLines_lines, [copy(s, splitLines_start+1, (splitLines_i - (splitLines_start)))]);
  splitLines_i := splitLines_i + 1;
  splitLines_start := splitLines_i;
end else begin
  splitLines_i := splitLines_i + 1;
end;
end;
  splitLines_lines := concat(splitLines_lines, [copy(s, splitLines_start+1, (Length(s) - (splitLines_start)))]);
  exit(splitLines_lines);
end;
function parseFasta(text: string): StrArray;
var
  parseFasta_key: string;
  parseFasta_val: string;
  parseFasta_out: array of string;
  parseFasta_line: string;
  parseFasta_hdr: string;
  parseFasta_idx: integer;
begin
  parseFasta_key := '';
  parseFasta_val := '';
  parseFasta_out := [];
  for parseFasta_line in splitLines(text) do begin
  if parseFasta_line = '' then begin
  continue;
end;
  if copy(parseFasta_line, 0+1, (1 - (0))) = '>' then begin
  if parseFasta_key <> '' then begin
  parseFasta_out := concat(parseFasta_out, [(parseFasta_key + ': ') + parseFasta_val]);
end;
  parseFasta_hdr := copy(parseFasta_line, 1+1, (Length(parseFasta_line) - (1)));
  parseFasta_idx := 0;
  while (parseFasta_idx < Length(parseFasta_hdr)) and (copy(parseFasta_hdr, parseFasta_idx+1, (parseFasta_idx + 1 - (parseFasta_idx))) <> ' ') do begin
  parseFasta_idx := parseFasta_idx + 1;
end;
  parseFasta_key := copy(parseFasta_hdr, 0+1, (parseFasta_idx - (0)));
  parseFasta_val := '';
end else begin
  if parseFasta_key = '' then begin
  writeln('missing header');
  exit([]);
end;
  parseFasta_val := parseFasta_val + parseFasta_line;
end;
end;
  if parseFasta_key <> '' then begin
  parseFasta_out := concat(parseFasta_out, [(parseFasta_key + ': ') + parseFasta_val]);
end;
  exit(parseFasta_out);
end;
procedure main();
var
  main_res: StrArray;
  main_line: string;
begin
  main_res := parseFasta(FASTA);
  for main_line in main_res do begin
  writeln(main_line);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  FASTA := (((('>Rosetta_Example_1' + #10 + '' + 'THERECANBENOSPACE' + #10 + '') + '>Rosetta_Example_2' + #10 + '') + 'THERECANBESEVERAL' + #10 + '') + 'LINESBUTTHEYALLMUST' + #10 + '') + 'BECONCATENATED';
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
