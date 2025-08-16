{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type StrArray = array of string;
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
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  LOWER: string;
  UPPER: string;
  PUNCT: string;
  corpus: string;
  idf_val: real;
  sep: string;
  idf: real;
  sub: string;
  text: string;
  smoothing: boolean;
  keep_newlines: boolean;
  n: integer;
  x: real;
  s: string;
  term: string;
  c: string;
  tf: integer;
  document: string;
  df: integer;
function to_lowercase(s: string): string; forward;
function is_punct(c: string): boolean; forward;
function clean_text(text: string; keep_newlines: boolean): string; forward;
function split(s: string; sep: string): StrArray; forward;
function contains(s: string; sub: string): boolean; forward;
function floor(x: real): real; forward;
function round3(x: real): real; forward;
function ln_(x: real): real; forward;
function log10(x: real): real; forward;
function term_frequency(term: string; document: string): integer; forward;
function document_frequency(term: string; corpus: string): IntArray; forward;
function inverse_document_frequency(df: integer; n: integer; smoothing: boolean): real; forward;
function tf_idf(tf: integer; idf: real): real; forward;
function to_lowercase(s: string): string;
var
  to_lowercase_res: string;
  to_lowercase_i: integer;
  to_lowercase_c: string;
  to_lowercase_j: integer;
  to_lowercase_found: boolean;
begin
  to_lowercase_res := '';
  to_lowercase_i := 0;
  while to_lowercase_i < Length(s) do begin
  to_lowercase_c := s[to_lowercase_i+1];
  to_lowercase_j := 0;
  to_lowercase_found := false;
  while to_lowercase_j < Length(UPPER) do begin
  if to_lowercase_c = UPPER[to_lowercase_j+1] then begin
  to_lowercase_res := to_lowercase_res + LOWER[to_lowercase_j+1];
  to_lowercase_found := true;
  break;
end;
  to_lowercase_j := to_lowercase_j + 1;
end;
  if not to_lowercase_found then begin
  to_lowercase_res := to_lowercase_res + to_lowercase_c;
end;
  to_lowercase_i := to_lowercase_i + 1;
end;
  exit(to_lowercase_res);
end;
function is_punct(c: string): boolean;
var
  is_punct_i: integer;
begin
  is_punct_i := 0;
  while is_punct_i < Length(PUNCT) do begin
  if c = PUNCT[is_punct_i+1] then begin
  exit(true);
end;
  is_punct_i := is_punct_i + 1;
end;
  exit(false);
end;
function clean_text(text: string; keep_newlines: boolean): string;
var
  clean_text_lower: string;
  clean_text_res: string;
  clean_text_i: integer;
  clean_text_ch: string;
begin
  clean_text_lower := to_lowercase(text);
  clean_text_res := '';
  clean_text_i := 0;
  while clean_text_i < Length(clean_text_lower) do begin
  clean_text_ch := clean_text_lower[clean_text_i+1];
  if is_punct(clean_text_ch) then begin
end else begin
  if clean_text_ch = '' + #10 + '' then begin
  if keep_newlines then begin
  clean_text_res := clean_text_res + '' + #10 + '';
end;
end else begin
  clean_text_res := clean_text_res + clean_text_ch;
end;
end;
  clean_text_i := clean_text_i + 1;
end;
  exit(clean_text_res);
end;
function split(s: string; sep: string): StrArray;
var
  split_res: array of string;
  split_current: string;
  split_i: integer;
  split_ch: string;
begin
  split_res := [];
  split_current := '';
  split_i := 0;
  while split_i < Length(s) do begin
  split_ch := s[split_i+1];
  if split_ch = sep then begin
  split_res := concat(split_res, StrArray([split_current]));
  split_current := '';
end else begin
  split_current := split_current + split_ch;
end;
  split_i := split_i + 1;
end;
  split_res := concat(split_res, StrArray([split_current]));
  exit(split_res);
end;
function contains(s: string; sub: string): boolean;
var
  contains_n: integer;
  contains_m: integer;
  contains_i: integer;
  contains_j: integer;
  contains_is_match: boolean;
begin
  contains_n := Length(s);
  contains_m := Length(sub);
  if contains_m = 0 then begin
  exit(true);
end;
  contains_i := 0;
  while contains_i <= (contains_n - contains_m) do begin
  contains_j := 0;
  contains_is_match := true;
  while contains_j < contains_m do begin
  if s[contains_i + contains_j+1] <> sub[contains_j+1] then begin
  contains_is_match := false;
  break;
end;
  contains_j := contains_j + 1;
end;
  if contains_is_match then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function floor(x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if Double(floor_i) > x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function round3(x: real): real;
begin
  exit(Floor((x * 1000) + 0.5) / 1000);
end;
function ln_(x: real): real;
var
  ln__t: real;
  ln__term: real;
  ln__sum: real;
  ln__k: integer;
begin
  ln__t := (x - 1) / (x + 1);
  ln__term := ln__t;
  ln__sum := 0;
  ln__k := 1;
  while ln__k <= 99 do begin
  ln__sum := ln__sum + (ln__term / Double(ln__k));
  ln__term := (ln__term * ln__t) * ln__t;
  ln__k := ln__k + 2;
end;
  exit(2 * ln__sum);
end;
function log10(x: real): real;
begin
  exit(ln(x) div ln(10));
end;
function term_frequency(term: string; document: string): integer;
var
  term_frequency_clean: string;
  term_frequency_tokens: StrArray;
  term_frequency_t: string;
  term_frequency_count: integer;
  term_frequency_i: integer;
begin
  term_frequency_clean := clean_text(document, false);
  term_frequency_tokens := split(term_frequency_clean, ' ');
  term_frequency_t := to_lowercase(term);
  term_frequency_count := 0;
  term_frequency_i := 0;
  while term_frequency_i < Length(term_frequency_tokens) do begin
  if (term_frequency_tokens[term_frequency_i] <> '') and (term_frequency_tokens[term_frequency_i] = term_frequency_t) then begin
  term_frequency_count := term_frequency_count + 1;
end;
  term_frequency_i := term_frequency_i + 1;
end;
  exit(term_frequency_count);
end;
function document_frequency(term: string; corpus: string): IntArray;
var
  document_frequency_clean: string;
  document_frequency_docs: StrArray;
  document_frequency_t: string;
  document_frequency_matches: integer;
  document_frequency_i: integer;
begin
  document_frequency_clean := clean_text(corpus, true);
  document_frequency_docs := split(document_frequency_clean, '' + #10 + '');
  document_frequency_t := to_lowercase(term);
  document_frequency_matches := 0;
  document_frequency_i := 0;
  while document_frequency_i < Length(document_frequency_docs) do begin
  if contains(document_frequency_docs[document_frequency_i], document_frequency_t) then begin
  document_frequency_matches := document_frequency_matches + 1;
end;
  document_frequency_i := document_frequency_i + 1;
end;
  exit([document_frequency_matches, Length(document_frequency_docs)]);
end;
function inverse_document_frequency(df: integer; n: integer; smoothing: boolean): real;
var
  inverse_document_frequency_ratio: real;
  inverse_document_frequency_l: real;
  inverse_document_frequency_result_: real;
  inverse_document_frequency_ratio_44: real;
  inverse_document_frequency_l_45: real;
  inverse_document_frequency_result__46: real;
begin
  if smoothing then begin
  if n = 0 then begin
  panic('log10(0) is undefined.');
end;
  inverse_document_frequency_ratio := Double(n) / (1 + Double(df));
  inverse_document_frequency_l := log10(inverse_document_frequency_ratio);
  inverse_document_frequency_result_ := round3(1 + inverse_document_frequency_l);
  writeln(inverse_document_frequency_result_);
  exit(inverse_document_frequency_result_);
end;
  if df = 0 then begin
  panic('df must be > 0');
end;
  if n = 0 then begin
  panic('log10(0) is undefined.');
end;
  inverse_document_frequency_ratio_44 := Double(n) / Double(df);
  inverse_document_frequency_l_45 := log10(inverse_document_frequency_ratio_44);
  inverse_document_frequency_result__46 := round3(inverse_document_frequency_l_45);
  writeln(inverse_document_frequency_result__46);
  exit(inverse_document_frequency_result__46);
end;
function tf_idf(tf: integer; idf: real): real;
var
  tf_idf_prod: real;
  tf_idf_result_: real;
begin
  tf_idf_prod := Double(tf) * idf;
  tf_idf_result_ := round3(tf_idf_prod);
  writeln(tf_idf_result_);
  exit(tf_idf_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  PUNCT := '!"#$%&''()*+,-./:;<=>?@[\]^_{|}~';
  writeln(term_frequency('to', 'To be, or not to be'));
  corpus := 'This is the first document in the corpus.' + #10 + 'ThIs is the second document in the corpus.' + #10 + 'THIS is the third document in the corpus.';
  writeln(list_int_to_str(document_frequency('first', corpus)));
  idf_val := inverse_document_frequency(1, 3, false);
  tf_idf(2, idf_val);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
