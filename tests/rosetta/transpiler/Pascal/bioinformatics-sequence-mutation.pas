{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type VariantArray = array of Variant;
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
  n: integer;
  le: integer;
  dna: string;
  w: array of integer;
function randInt(s: integer; n: integer): IntArray; forward;
function padLeft(s: string; w: integer): string; forward;
function makeSeq(s: integer; le: integer): VariantArray; forward;
function mutate(s: integer; dna: string; w: IntArray): VariantArray; forward;
procedure prettyPrint(dna: string; rowLen: integer); forward;
function wstring(w: IntArray): string; forward;
procedure main(); forward;
function randInt(s: integer; n: integer): IntArray;
var
  randInt_next: integer;
begin
  randInt_next := ((s * 1664525) + 1013904223) mod 2147483647;
  exit([randInt_next, randInt_next mod n]);
end;
function padLeft(s: string; w: integer): string;
var
  padLeft_res: string;
begin
  padLeft_res := '';
  n := w - Length(s);
  while n > 0 do begin
  padLeft_res := padLeft_res + ' ';
  n := n - 1;
end;
  exit(padLeft_res + s);
end;
function makeSeq(s: integer; le: integer): VariantArray;
var
  makeSeq_bases: string;
  makeSeq_out: string;
  makeSeq_i: integer;
  makeSeq_r: IntArray;
  makeSeq_idx: integer;
begin
  makeSeq_bases := 'ACGT';
  makeSeq_out := '';
  makeSeq_i := 0;
  while makeSeq_i < le do begin
  makeSeq_r := randInt(s, 4);
  s := makeSeq_r[0];
  makeSeq_idx := Trunc(makeSeq_r[1]);
  makeSeq_out := makeSeq_out + copy(makeSeq_bases, makeSeq_idx+1, (makeSeq_idx + 1 - (makeSeq_idx)));
  makeSeq_i := makeSeq_i + 1;
end;
  exit([s, makeSeq_out]);
end;
function mutate(s: integer; dna: string; w: IntArray): VariantArray;
var
  mutate_bases: string;
  mutate_r: IntArray;
  mutate_p: integer;
  mutate_x: integer;
  mutate_arr: array of string;
  mutate_i: integer;
  mutate_idx: integer;
  mutate_b: string;
  mutate_j: integer;
  mutate_idx2: integer;
  mutate_out: string;
begin
  mutate_bases := 'ACGT';
  le := Length(dna);
  mutate_r := randInt(s, le);
  s := mutate_r[0];
  mutate_p := Trunc(mutate_r[1]);
  mutate_r := randInt(s, 300);
  s := mutate_r[0];
  mutate_x := Trunc(mutate_r[1]);
  mutate_arr := [];
  mutate_i := 0;
  while mutate_i < le do begin
  mutate_arr := concat(mutate_arr, [copy(dna, mutate_i+1, (mutate_i + 1 - (mutate_i)))]);
  mutate_i := mutate_i + 1;
end;
  if mutate_x < w[0] then begin
  mutate_r := randInt(s, 4);
  s := mutate_r[0];
  mutate_idx := Trunc(mutate_r[1]);
  mutate_b := copy(mutate_bases, mutate_idx+1, (mutate_idx + 1 - (mutate_idx)));
  writeln(((((('  Change @' + padLeft(IntToStr(mutate_p), 3)) + ' ''') + mutate_arr[mutate_p]) + ''' to ''') + mutate_b) + '''');
  mutate_arr[mutate_p] := mutate_b;
end else begin
  if mutate_x < (w[0] + w[1]) then begin
  writeln(((('  Delete @' + padLeft(IntToStr(mutate_p), 3)) + ' ''') + mutate_arr[mutate_p]) + '''');
  mutate_j := mutate_p;
  while mutate_j < (Length(mutate_arr) - 1) do begin
  mutate_arr[mutate_j] := mutate_arr[mutate_j + 1];
  mutate_j := mutate_j + 1;
end;
  mutate_arr := copy(mutate_arr, 0, (Length(mutate_arr) - 1 - (0)));
end else begin
  mutate_r := randInt(s, 4);
  s := mutate_r[0];
  mutate_idx2 := Trunc(mutate_r[1]);
  mutate_b := copy(mutate_bases, mutate_idx2+1, (mutate_idx2 + 1 - (mutate_idx2)));
  mutate_arr := concat(mutate_arr, ['']);
  mutate_j := Length(mutate_arr) - 1;
  while mutate_j > mutate_p do begin
  mutate_arr[mutate_j] := mutate_arr[mutate_j - 1];
  mutate_j := mutate_j - 1;
end;
  writeln(((('  Insert @' + padLeft(IntToStr(mutate_p), 3)) + ' ''') + mutate_b) + '''');
  mutate_arr[mutate_p] := mutate_b;
end;
end;
  mutate_out := '';
  mutate_i := 0;
  while mutate_i < Length(mutate_arr) do begin
  mutate_out := mutate_out + mutate_arr[mutate_i];
  mutate_i := mutate_i + 1;
end;
  exit([s, mutate_out]);
end;
procedure prettyPrint(dna: string; rowLen: integer);
var
  prettyPrint_i: integer;
  prettyPrint_k: integer;
  prettyPrint_a: integer;
  prettyPrint_c: integer;
  prettyPrint_g: integer;
  prettyPrint_t: integer;
  prettyPrint_idx: integer;
  prettyPrint_ch: string;
begin
  writeln('SEQUENCE:');
  le := Length(dna);
  prettyPrint_i := 0;
  while prettyPrint_i < le do begin
  prettyPrint_k := prettyPrint_i + rowLen;
  if prettyPrint_k > le then begin
  prettyPrint_k := le;
end;
  writeln((padLeft(IntToStr(prettyPrint_i), 5) + ': ') + copy(dna, prettyPrint_i+1, (prettyPrint_k - (prettyPrint_i))));
  prettyPrint_i := prettyPrint_i + rowLen;
end;
  prettyPrint_a := 0;
  prettyPrint_c := 0;
  prettyPrint_g := 0;
  prettyPrint_t := 0;
  prettyPrint_idx := 0;
  while prettyPrint_idx < le do begin
  prettyPrint_ch := copy(dna, prettyPrint_idx+1, (prettyPrint_idx + 1 - (prettyPrint_idx)));
  if prettyPrint_ch = 'A' then begin
  prettyPrint_a := prettyPrint_a + 1;
end else begin
  if prettyPrint_ch = 'C' then begin
  prettyPrint_c := prettyPrint_c + 1;
end else begin
  if prettyPrint_ch = 'G' then begin
  prettyPrint_g := prettyPrint_g + 1;
end else begin
  if prettyPrint_ch = 'T' then begin
  prettyPrint_t := prettyPrint_t + 1;
end;
end;
end;
end;
  prettyPrint_idx := prettyPrint_idx + 1;
end;
  writeln('');
  writeln('BASE COUNT:');
  writeln('    A: ' + padLeft(IntToStr(prettyPrint_a), 3));
  writeln('    C: ' + padLeft(IntToStr(prettyPrint_c), 3));
  writeln('    G: ' + padLeft(IntToStr(prettyPrint_g), 3));
  writeln('    T: ' + padLeft(IntToStr(prettyPrint_t), 3));
  writeln('    ------');
  writeln('    Σ: ' + IntToStr(le));
  writeln('    ======');
end;
function wstring(w: IntArray): string;
begin
  exit(((((('  Change: ' + IntToStr(w[0])) + '' + #10 + '  Delete: ') + IntToStr(w[1])) + '' + #10 + '  Insert: ') + IntToStr(w[2])) + '' + #10 + '');
end;
procedure main();
var
  main_seed: integer;
  main_res: VariantArray;
  main_muts: integer;
  main_i: integer;
begin
  main_seed := 1;
  main_res := makeSeq(main_seed, 250);
  main_seed := main_res[0];
  dna := main_res[1];
  prettyPrint(dna, 50);
  main_muts := 10;
  w := [100, 100, 100];
  writeln('' + #10 + 'WEIGHTS (ex 300):');
  writeln(wstring(w));
  writeln(('MUTATIONS (' + IntToStr(main_muts)) + '):');
  main_i := 0;
  while main_i < main_muts do begin
  main_res := mutate(main_seed, dna, w);
  main_seed := main_res[0];
  dna := main_res[1];
  main_i := main_i + 1;
end;
  writeln('');
  prettyPrint(dna, 50);
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
