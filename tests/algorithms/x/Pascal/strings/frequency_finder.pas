{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  ETAOIN: string;
  LETTERS: string;
function etaoin_index(etaoin_index_letter: string): integer; forward;
function get_letter_count(get_letter_count_message: string): specialize TFPGMap<string, integer>; forward;
function get_frequency_order(get_frequency_order_message: string): string; forward;
function english_freq_match_score(english_freq_match_score_message: string): integer; forward;
procedure main(); forward;
function etaoin_index(etaoin_index_letter: string): integer;
var
  etaoin_index_i: integer;
begin
  etaoin_index_i := 0;
  while etaoin_index_i < Length(ETAOIN) do begin
  if copy(ETAOIN, etaoin_index_i+1, (etaoin_index_i + 1 - (etaoin_index_i))) = etaoin_index_letter then begin
  exit(etaoin_index_i);
end;
  etaoin_index_i := etaoin_index_i + 1;
end;
  exit(Length(ETAOIN));
end;
function get_letter_count(get_letter_count_message: string): specialize TFPGMap<string, integer>;
var
  get_letter_count_letter_count: specialize TFPGMap<string, integer>;
  get_letter_count_i: integer;
  get_letter_count_c: string;
  get_letter_count_msg: string;
  get_letter_count_j: integer;
  get_letter_count_ch: string;
begin
  get_letter_count_letter_count := specialize TFPGMap<string, integer>.Create();
  get_letter_count_i := 0;
  while get_letter_count_i < Length(LETTERS) do begin
  get_letter_count_c := copy(LETTERS, get_letter_count_i+1, (get_letter_count_i + 1 - (get_letter_count_i)));
  get_letter_count_letter_count[get_letter_count_c] := 0;
  get_letter_count_i := get_letter_count_i + 1;
end;
  get_letter_count_msg := UpperCase(get_letter_count_message);
  get_letter_count_j := 0;
  while get_letter_count_j < Length(get_letter_count_msg) do begin
  get_letter_count_ch := copy(get_letter_count_msg, get_letter_count_j+1, (get_letter_count_j + 1 - (get_letter_count_j)));
  if Pos(get_letter_count_ch, LETTERS) <> 0 then begin
  get_letter_count_letter_count[get_letter_count_ch] := get_letter_count_letter_count[get_letter_count_ch] + 1;
end;
  get_letter_count_j := get_letter_count_j + 1;
end;
  exit(get_letter_count_letter_count);
end;
function get_frequency_order(get_frequency_order_message: string): string;
var
  get_frequency_order_letter_to_freq: specialize TFPGMap<string, integer>;
  get_frequency_order_max_freq: integer;
  get_frequency_order_i: integer;
  get_frequency_order_letter: string;
  get_frequency_order_f: integer;
  get_frequency_order_f_idx: integer;
  get_frequency_order_result_: string;
  get_frequency_order_freq: integer;
  get_frequency_order_group: array of string;
  get_frequency_order_j: integer;
  get_frequency_order_g_len: integer;
  get_frequency_order_a: integer;
  get_frequency_order_b: integer;
  get_frequency_order_g1: string;
  get_frequency_order_g2: string;
  get_frequency_order_idx1: integer;
  get_frequency_order_idx2: integer;
  get_frequency_order_tmp: string;
  get_frequency_order_g: integer;
begin
  get_frequency_order_letter_to_freq := get_letter_count(get_frequency_order_message);
  get_frequency_order_max_freq := 0;
  get_frequency_order_i := 0;
  while get_frequency_order_i < Length(LETTERS) do begin
  get_frequency_order_letter := copy(LETTERS, get_frequency_order_i+1, (get_frequency_order_i + 1 - (get_frequency_order_i)));
  get_frequency_order_f_idx := get_frequency_order_letter_to_freq.IndexOf(get_frequency_order_letter);
  if get_frequency_order_f_idx <> -1 then begin
  get_frequency_order_f := get_frequency_order_letter_to_freq.Data[get_frequency_order_f_idx];
end else begin
  get_frequency_order_f := 0;
end;
  if get_frequency_order_f > get_frequency_order_max_freq then begin
  get_frequency_order_max_freq := get_frequency_order_f;
end;
  get_frequency_order_i := get_frequency_order_i + 1;
end;
  get_frequency_order_result_ := '';
  get_frequency_order_freq := get_frequency_order_max_freq;
  while get_frequency_order_freq >= 0 do begin
  get_frequency_order_group := [];
  get_frequency_order_j := 0;
  while get_frequency_order_j < Length(LETTERS) do begin
  get_frequency_order_letter := copy(LETTERS, get_frequency_order_j+1, (get_frequency_order_j + 1 - (get_frequency_order_j)));
  if get_frequency_order_letter_to_freq[get_frequency_order_letter] = get_frequency_order_freq then begin
  get_frequency_order_group := concat(get_frequency_order_group, StrArray([get_frequency_order_letter]));
end;
  get_frequency_order_j := get_frequency_order_j + 1;
end;
  get_frequency_order_g_len := Length(get_frequency_order_group);
  get_frequency_order_a := 0;
  while get_frequency_order_a < get_frequency_order_g_len do begin
  get_frequency_order_b := 0;
  while get_frequency_order_b < ((get_frequency_order_g_len - get_frequency_order_a) - 1) do begin
  get_frequency_order_g1 := get_frequency_order_group[get_frequency_order_b];
  get_frequency_order_g2 := get_frequency_order_group[get_frequency_order_b + 1];
  get_frequency_order_idx1 := etaoin_index(get_frequency_order_g1);
  get_frequency_order_idx2 := etaoin_index(get_frequency_order_g2);
  if get_frequency_order_idx1 < get_frequency_order_idx2 then begin
  get_frequency_order_tmp := get_frequency_order_group[get_frequency_order_b];
  get_frequency_order_group[get_frequency_order_b] := get_frequency_order_group[get_frequency_order_b + 1];
  get_frequency_order_group[get_frequency_order_b + 1] := get_frequency_order_tmp;
end;
  get_frequency_order_b := get_frequency_order_b + 1;
end;
  get_frequency_order_a := get_frequency_order_a + 1;
end;
  get_frequency_order_g := 0;
  while get_frequency_order_g < Length(get_frequency_order_group) do begin
  get_frequency_order_result_ := get_frequency_order_result_ + get_frequency_order_group[get_frequency_order_g];
  get_frequency_order_g := get_frequency_order_g + 1;
end;
  get_frequency_order_freq := get_frequency_order_freq - 1;
end;
  exit(get_frequency_order_result_);
end;
function english_freq_match_score(english_freq_match_score_message: string): integer;
var
  english_freq_match_score_freq_order: string;
  english_freq_match_score_top: string;
  english_freq_match_score_bottom: string;
  english_freq_match_score_score: integer;
  english_freq_match_score_i: integer;
  english_freq_match_score_c: string;
  english_freq_match_score_j: integer;
begin
  english_freq_match_score_freq_order := get_frequency_order(english_freq_match_score_message);
  english_freq_match_score_top := copy(english_freq_match_score_freq_order, 1, 6);
  english_freq_match_score_bottom := copy(english_freq_match_score_freq_order, Length(english_freq_match_score_freq_order) - 6+1, (Length(english_freq_match_score_freq_order) - (Length(english_freq_match_score_freq_order) - 6)));
  english_freq_match_score_score := 0;
  english_freq_match_score_i := 0;
  while english_freq_match_score_i < 6 do begin
  english_freq_match_score_c := copy(ETAOIN, english_freq_match_score_i+1, (english_freq_match_score_i + 1 - (english_freq_match_score_i)));
  if Pos(english_freq_match_score_c, english_freq_match_score_top) <> 0 then begin
  english_freq_match_score_score := english_freq_match_score_score + 1;
end;
  english_freq_match_score_i := english_freq_match_score_i + 1;
end;
  english_freq_match_score_j := Length(ETAOIN) - 6;
  while english_freq_match_score_j < Length(ETAOIN) do begin
  english_freq_match_score_c := copy(ETAOIN, english_freq_match_score_j+1, (english_freq_match_score_j + 1 - (english_freq_match_score_j)));
  if Pos(english_freq_match_score_c, english_freq_match_score_bottom) <> 0 then begin
  english_freq_match_score_score := english_freq_match_score_score + 1;
end;
  english_freq_match_score_j := english_freq_match_score_j + 1;
end;
  exit(english_freq_match_score_score);
end;
procedure main();
begin
  writeln(get_frequency_order('Hello World'));
  writeln(english_freq_match_score('Hello World'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ETAOIN := 'ETAOINSHRDLCUMWFGYPBVKJXQZ';
  LETTERS := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
