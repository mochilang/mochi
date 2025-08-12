{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type TextCounts = record
  single: specialize TFPGMap<string, integer>;
  double: specialize TFPGMap<string, integer>;
end;
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
  calculate_entropy_ch_idx: integer;
  calculate_entropy_seq_idx: integer;
  text1: string;
  text3: string;
  text: string;
  x: real;
function makeTextCounts(single: specialize TFPGMap<string, integer>; double: specialize TFPGMap<string, integer>): TextCounts; forward;
function log2(x: real): real; forward;
function analyze_text(text: string): TextCounts; forward;
function round_to_int(x: real): integer; forward;
procedure calculate_entropy(text: string); forward;
function makeTextCounts(single: specialize TFPGMap<string, integer>; double: specialize TFPGMap<string, integer>): TextCounts;
begin
  Result.single := single;
  Result.double := double;
end;
function log2(x: real): real;
var
  log2_k: real;
  log2_v: real;
  log2_z: real;
  log2_zpow: real;
  log2_sum: real;
  log2_i: integer;
  log2_ln2: real;
begin
  log2_k := 0;
  log2_v := x;
  while log2_v >= 2 do begin
  log2_v := log2_v / 2;
  log2_k := log2_k + 1;
end;
  while log2_v < 1 do begin
  log2_v := log2_v * 2;
  log2_k := log2_k - 1;
end;
  log2_z := (log2_v - 1) / (log2_v + 1);
  log2_zpow := log2_z;
  log2_sum := log2_z;
  log2_i := 3;
  while log2_i <= 9 do begin
  log2_zpow := (log2_zpow * log2_z) * log2_z;
  log2_sum := log2_sum + (log2_zpow / Double(log2_i));
  log2_i := log2_i + 2;
end;
  log2_ln2 := 0.6931471805599453;
  exit(log2_k + ((2 * log2_sum) / log2_ln2));
end;
function analyze_text(text: string): TextCounts;
var
  analyze_text_single: specialize TFPGMap<string, integer>;
  analyze_text_double: specialize TFPGMap<string, integer>;
  analyze_text_n: integer;
  analyze_text_last: string;
  analyze_text_first: string;
  analyze_text_pair0: string;
  analyze_text_i: integer;
  analyze_text_ch: string;
  analyze_text_seq: string;
begin
  analyze_text_single := specialize TFPGMap<string, integer>.Create();
  analyze_text_double := specialize TFPGMap<string, integer>.Create();
  analyze_text_n := Length(text);
  if analyze_text_n = 0 then begin
  exit(makeTextCounts(analyze_text_single, analyze_text_double));
end;
  analyze_text_last := copy(text, analyze_text_n - 1+1, (analyze_text_n - (analyze_text_n - 1)));
  if analyze_text_single.IndexOf(analyze_text_last) <> -1 then begin
  analyze_text_single[analyze_text_last] := analyze_text_single[analyze_text_last] + 1;
end else begin
  analyze_text_single[analyze_text_last] := 1;
end;
  analyze_text_first := copy(text, 1, 1);
  analyze_text_pair0 := ' ' + analyze_text_first;
  analyze_text_double[analyze_text_pair0] := 1;
  analyze_text_i := 0;
  while analyze_text_i < (analyze_text_n - 1) do begin
  analyze_text_ch := copy(text, analyze_text_i+1, (analyze_text_i + 1 - (analyze_text_i)));
  if analyze_text_single.IndexOf(analyze_text_ch) <> -1 then begin
  analyze_text_single[analyze_text_ch] := analyze_text_single[analyze_text_ch] + 1;
end else begin
  analyze_text_single[analyze_text_ch] := 1;
end;
  analyze_text_seq := copy(text, analyze_text_i+1, (analyze_text_i + 2 - (analyze_text_i)));
  if analyze_text_double.IndexOf(analyze_text_seq) <> -1 then begin
  analyze_text_double[analyze_text_seq] := analyze_text_double[analyze_text_seq] + 1;
end else begin
  analyze_text_double[analyze_text_seq] := 1;
end;
  analyze_text_i := analyze_text_i + 1;
end;
  exit(makeTextCounts(analyze_text_single, analyze_text_double));
end;
function round_to_int(x: real): integer;
begin
  if x < 0 then begin
  exit(Trunc(x - 0.5));
end;
  exit(Trunc(x + 0.5));
end;
procedure calculate_entropy(text: string);
var
  calculate_entropy_counts: TextCounts;
  calculate_entropy_alphas: string;
  calculate_entropy_total1: integer;
  calculate_entropy_ch: string;
  calculate_entropy_h1: real;
  calculate_entropy_i: integer;
  calculate_entropy_prob: real;
  calculate_entropy_first_entropy: real;
  calculate_entropy_total2: integer;
  calculate_entropy_seq: string;
  calculate_entropy_h2: real;
  calculate_entropy_a0: integer;
  calculate_entropy_ch0: string;
  calculate_entropy_a1: integer;
  calculate_entropy_ch1: string;
  calculate_entropy_second_entropy: real;
  calculate_entropy_diff: real;
begin
  calculate_entropy_counts := analyze_text(text);
  calculate_entropy_alphas := ' abcdefghijklmnopqrstuvwxyz';
  calculate_entropy_total1 := 0;
  for calculate_entropy_ch_idx := 0 to (calculate_entropy_counts.single.Count - 1) do begin
  calculate_entropy_ch := calculate_entropy_counts.single.Keys[calculate_entropy_ch_idx];
  calculate_entropy_total1 := calculate_entropy_total1 + calculate_entropy_counts.single[calculate_entropy_ch];
end;
  calculate_entropy_h1 := 0;
  calculate_entropy_i := 0;
  while calculate_entropy_i < Length(calculate_entropy_alphas) do begin
  calculate_entropy_ch := copy(calculate_entropy_alphas, calculate_entropy_i+1, (calculate_entropy_i + 1 - (calculate_entropy_i)));
  if calculate_entropy_counts.single.IndexOf(calculate_entropy_ch) <> -1 then begin
  calculate_entropy_prob := Double(calculate_entropy_counts.single[calculate_entropy_ch]) / Double(calculate_entropy_total1);
  calculate_entropy_h1 := calculate_entropy_h1 + (calculate_entropy_prob * log2(calculate_entropy_prob));
end;
  calculate_entropy_i := calculate_entropy_i + 1;
end;
  calculate_entropy_first_entropy := -calculate_entropy_h1;
  writeln(IntToStr(round_to_int(calculate_entropy_first_entropy)) + '.0');
  calculate_entropy_total2 := 0;
  for calculate_entropy_seq_idx := 0 to (calculate_entropy_counts.double.Count - 1) do begin
  calculate_entropy_seq := calculate_entropy_counts.double.Keys[calculate_entropy_seq_idx];
  calculate_entropy_total2 := calculate_entropy_total2 + calculate_entropy_counts.double[calculate_entropy_seq];
end;
  calculate_entropy_h2 := 0;
  calculate_entropy_a0 := 0;
  while calculate_entropy_a0 < Length(calculate_entropy_alphas) do begin
  calculate_entropy_ch0 := copy(calculate_entropy_alphas, calculate_entropy_a0+1, (calculate_entropy_a0 + 1 - (calculate_entropy_a0)));
  calculate_entropy_a1 := 0;
  while calculate_entropy_a1 < Length(calculate_entropy_alphas) do begin
  calculate_entropy_ch1 := copy(calculate_entropy_alphas, calculate_entropy_a1+1, (calculate_entropy_a1 + 1 - (calculate_entropy_a1)));
  calculate_entropy_seq := calculate_entropy_ch0 + calculate_entropy_ch1;
  if calculate_entropy_counts.double.IndexOf(calculate_entropy_seq) <> -1 then begin
  calculate_entropy_prob := Double(calculate_entropy_counts.double[calculate_entropy_seq]) / Double(calculate_entropy_total2);
  calculate_entropy_h2 := calculate_entropy_h2 + (calculate_entropy_prob * log2(calculate_entropy_prob));
end;
  calculate_entropy_a1 := calculate_entropy_a1 + 1;
end;
  calculate_entropy_a0 := calculate_entropy_a0 + 1;
end;
  calculate_entropy_second_entropy := -calculate_entropy_h2;
  writeln(IntToStr(round_to_int(calculate_entropy_second_entropy)) + '.0');
  calculate_entropy_diff := calculate_entropy_second_entropy - calculate_entropy_first_entropy;
  writeln(IntToStr(round_to_int(calculate_entropy_diff)) + '.0');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  text1 := ('Behind Winston''s back the voice ' + 'from the telescreen was still ') + 'babbling and the overfulfilment';
  calculate_entropy(text1);
  text3 := ((((((((('Had repulsive dashwoods suspicion sincerity but advantage now him. ' + 'Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. ') + 'You greatest jointure saw horrible. He private he on be imagine ') + 'suppose. Fertile beloved evident through no service elderly is. Blind ') + 'there if every no so at. Own neglected you preferred way sincerity ') + 'delivered his attempted. To of message cottage windows do besides ') + 'against uncivil.  Delightful unreserved impossible few estimating ') + 'men favourable see entreaties. She propriety immediate was improving. ') + 'He or entrance humoured likewise moderate. Much nor game son say ') + 'feel. Fat make met can must form into gate. Me we offending prevailed ') + 'discovery.';
  calculate_entropy(text3);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
