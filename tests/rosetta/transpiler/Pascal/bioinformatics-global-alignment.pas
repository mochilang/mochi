{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type StrArrayArray = array of StrArray;
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
  s: string;
  start: integer;
  s1: string;
  ss: StrArray;
  ch: string;
function padLeft(s: string; w: integer): string; forward;
function indexOfFrom(s: string; ch: string; start: integer): integer; forward;
function containsStr(s: string; sub: string): boolean; forward;
function distinct(slist: StrArray): StrArray; forward;
function permutations(xs: StrArray): StrArrayArray; forward;
function headTailOverlap(s1: string; s2: string): integer; forward;
function deduplicate(slist: StrArray): StrArray; forward;
function joinAll(ss: StrArray): string; forward;
function shortestCommonSuperstring(slist: StrArray): string; forward;
procedure printCounts(seq: string); forward;
procedure main(); forward;
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
function indexOfFrom(s: string; ch: string; start: integer): integer;
var
  indexOfFrom_i: integer;
begin
  indexOfFrom_i := start;
  while indexOfFrom_i < Length(s) do begin
  if copy(s, indexOfFrom_i+1, (indexOfFrom_i + 1 - (indexOfFrom_i))) = ch then begin
  exit(indexOfFrom_i);
end;
  indexOfFrom_i := indexOfFrom_i + 1;
end;
  exit(-1);
end;
function containsStr(s: string; sub: string): boolean;
var
  containsStr_i: integer;
  containsStr_sl: integer;
  containsStr_subl: integer;
begin
  containsStr_i := 0;
  containsStr_sl := Length(s);
  containsStr_subl := Length(sub);
  while containsStr_i <= (containsStr_sl - containsStr_subl) do begin
  if copy(s, containsStr_i+1, (containsStr_i + containsStr_subl - (containsStr_i))) = sub then begin
  exit(true);
end;
  containsStr_i := containsStr_i + 1;
end;
  exit(false);
end;
function distinct(slist: StrArray): StrArray;
var
  distinct_res: array of string;
  distinct_found: boolean;
  distinct_r: string;
begin
  distinct_res := [];
  for s in slist do begin
  distinct_found := false;
  for distinct_r in distinct_res do begin
  if distinct_r = s then begin
  distinct_found := true;
  break;
end;
end;
  if not distinct_found then begin
  distinct_res := concat(distinct_res, [s]);
end;
end;
  exit(distinct_res);
end;
function permutations(xs: StrArray): StrArrayArray;
var
  permutations_res: array of StrArray;
  permutations_i: integer;
  permutations_rest: array of string;
  permutations_j: integer;
  permutations_subs: array of StrArray;
  permutations_p: StrArray;
  permutations_perm: array of string;
  permutations_k: integer;
begin
  if Length(xs) <= 1 then begin
  exit([xs]);
end;
  permutations_res := [];
  permutations_i := 0;
  while permutations_i < Length(xs) do begin
  permutations_rest := [];
  permutations_j := 0;
  while permutations_j < Length(xs) do begin
  if permutations_j <> permutations_i then begin
  permutations_rest := concat(permutations_rest, [xs[permutations_j]]);
end;
  permutations_j := permutations_j + 1;
end;
  permutations_subs := permutations(permutations_rest);
  for permutations_p in permutations_subs do begin
  permutations_perm := [xs[permutations_i]];
  permutations_k := 0;
  while permutations_k < Length(permutations_p) do begin
  permutations_perm := concat(permutations_perm, [permutations_p[permutations_k]]);
  permutations_k := permutations_k + 1;
end;
  permutations_res := concat(permutations_res, [permutations_perm]);
end;
  permutations_i := permutations_i + 1;
end;
  exit(permutations_res);
end;
function headTailOverlap(s1: string; s2: string): integer;
var
  headTailOverlap_ix: integer;
  headTailOverlap_sublen: integer;
begin
  start := 0;
  while true do begin
  headTailOverlap_ix := indexOfFrom(s1, copy(s2, 0+1, (1 - (0))), start);
  if headTailOverlap_ix = (0 - 1) then begin
  exit(0);
end;
  start := headTailOverlap_ix;
  headTailOverlap_sublen := Length(s1) - start;
  if headTailOverlap_sublen > Length(s2) then begin
  headTailOverlap_sublen := Length(s2);
end;
  if copy(s2, 0+1, (headTailOverlap_sublen - (0))) = copy(s1, start+1, (start + headTailOverlap_sublen - (start))) then begin
  exit(headTailOverlap_sublen);
end;
  start := start + 1;
end;
end;
function deduplicate(slist: StrArray): StrArray;
var
  deduplicate_arr: StrArray;
  deduplicate_filtered: array of string;
  deduplicate_i: integer;
  deduplicate_within: boolean;
  deduplicate_j: integer;
begin
  deduplicate_arr := distinct(slist);
  deduplicate_filtered := [];
  deduplicate_i := 0;
  while deduplicate_i < Length(deduplicate_arr) do begin
  s1 := deduplicate_arr[deduplicate_i];
  deduplicate_within := false;
  deduplicate_j := 0;
  while deduplicate_j < Length(deduplicate_arr) do begin
  if (deduplicate_j <> deduplicate_i) and containsStr(deduplicate_arr[deduplicate_j], s1) then begin
  deduplicate_within := true;
  break;
end;
  deduplicate_j := deduplicate_j + 1;
end;
  if not deduplicate_within then begin
  deduplicate_filtered := concat(deduplicate_filtered, [s1]);
end;
  deduplicate_i := deduplicate_i + 1;
end;
  exit(deduplicate_filtered);
end;
function joinAll(ss: StrArray): string;
var
  joinAll_out: string;
begin
  joinAll_out := '';
  for s in ss do begin
  joinAll_out := joinAll_out + s;
end;
  exit(joinAll_out);
end;
function shortestCommonSuperstring(slist: StrArray): string;
var
  shortestCommonSuperstring_shortest: string;
  shortestCommonSuperstring_perms: StrArrayArray;
  shortestCommonSuperstring_idx: integer;
  shortestCommonSuperstring_perm: array of string;
  shortestCommonSuperstring_sup: string;
  shortestCommonSuperstring_i: integer;
  shortestCommonSuperstring_ov: integer;
begin
  ss := deduplicate(slist);
  shortestCommonSuperstring_shortest := joinAll(ss);
  shortestCommonSuperstring_perms := permutations(ss);
  shortestCommonSuperstring_idx := 0;
  while shortestCommonSuperstring_idx < Length(shortestCommonSuperstring_perms) do begin
  shortestCommonSuperstring_perm := shortestCommonSuperstring_perms[shortestCommonSuperstring_idx];
  shortestCommonSuperstring_sup := shortestCommonSuperstring_perm[0];
  shortestCommonSuperstring_i := 0;
  while shortestCommonSuperstring_i < (Length(ss) - 1) do begin
  shortestCommonSuperstring_ov := headTailOverlap(shortestCommonSuperstring_perm[shortestCommonSuperstring_i], shortestCommonSuperstring_perm[shortestCommonSuperstring_i + 1]);
  shortestCommonSuperstring_sup := shortestCommonSuperstring_sup + copy(shortestCommonSuperstring_perm[shortestCommonSuperstring_i + 1], shortestCommonSuperstring_ov+1, (Length(shortestCommonSuperstring_perm[shortestCommonSuperstring_i + 1]) - (shortestCommonSuperstring_ov)));
  shortestCommonSuperstring_i := shortestCommonSuperstring_i + 1;
end;
  if Length(shortestCommonSuperstring_sup) < Length(shortestCommonSuperstring_shortest) then begin
  shortestCommonSuperstring_shortest := shortestCommonSuperstring_sup;
end;
  shortestCommonSuperstring_idx := shortestCommonSuperstring_idx + 1;
end;
  exit(shortestCommonSuperstring_shortest);
end;
procedure printCounts(seq: string);
var
  printCounts_a: integer;
  printCounts_c: integer;
  printCounts_g: integer;
  printCounts_t: integer;
  printCounts_i: integer;
  printCounts_total: integer;
begin
  printCounts_a := 0;
  printCounts_c := 0;
  printCounts_g := 0;
  printCounts_t := 0;
  printCounts_i := 0;
  while printCounts_i < Length(seq) do begin
  ch := copy(seq, printCounts_i+1, (printCounts_i + 1 - (printCounts_i)));
  if ch = 'A' then begin
  printCounts_a := printCounts_a + 1;
end else begin
  if ch = 'C' then begin
  printCounts_c := printCounts_c + 1;
end else begin
  if ch = 'G' then begin
  printCounts_g := printCounts_g + 1;
end else begin
  if ch = 'T' then begin
  printCounts_t := printCounts_t + 1;
end;
end;
end;
end;
  printCounts_i := printCounts_i + 1;
end;
  printCounts_total := Length(seq);
  writeln(('' + #10 + 'Nucleotide counts for ' + seq) + ':' + #10 + '');
  writeln(padLeft('A', 10) + padLeft(IntToStr(printCounts_a), 12));
  writeln(padLeft('C', 10) + padLeft(IntToStr(printCounts_c), 12));
  writeln(padLeft('G', 10) + padLeft(IntToStr(printCounts_g), 12));
  writeln(padLeft('T', 10) + padLeft(IntToStr(printCounts_t), 12));
  writeln(padLeft('Other', 10) + padLeft(IntToStr(printCounts_total - (((printCounts_a + printCounts_c) + printCounts_g) + printCounts_t)), 12));
  writeln('  ____________________');
  writeln(padLeft('Total length', 14) + padLeft(IntToStr(printCounts_total), 8));
end;
procedure main();
var
  main_tests: array of StrArray;
  main_seqs: StrArray;
  main_scs: string;
begin
  main_tests := [['TA', 'AAG', 'TA', 'GAA', 'TA'], ['CATTAGGG', 'ATTAG', 'GGG', 'TA'], ['AAGAUGGA', 'GGAGCGCAUC', 'AUCGCAAUAAGGA'], ['ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT', 'GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT', 'CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA', 'TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT', 'GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC', 'CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT', 'TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC', 'GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT', 'TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC', 'CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA', 'TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA']];
  for main_seqs in main_tests do begin
  main_scs := shortestCommonSuperstring(main_seqs);
  printCounts(main_scs);
end;
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
