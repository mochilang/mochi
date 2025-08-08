{$mode objfpc}
program Main;
uses SysUtils;
type Token = record
  offset: integer;
  length_: integer;
  indicator: string;
end;
type TokenArray = array of Token;
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
  c1: TokenArray;
  c2: TokenArray;
  tokens_example: array of Token;
  text: string;
  window: string;
  search_buffer: string;
  window_index: integer;
  tokens: TokenArray;
  t: Token;
  ts: TokenArray;
  text_index: integer;
  window_size: integer;
  lookahead: integer;
function makeToken(offset: integer; length_: integer; indicator: string): Token; forward;
function token_to_string(t: Token): string; forward;
function tokens_to_string(ts: TokenArray): string; forward;
function match_length_from_index(text: string; window: string; text_index: integer; window_index: integer): integer; forward;
function find_encoding_token(text: string; search_buffer: string): Token; forward;
function lz77_compress(text: string; window_size: integer; lookahead: integer): TokenArray; forward;
function lz77_decompress(tokens: TokenArray): string; forward;
function makeToken(offset: integer; length_: integer; indicator: string): Token;
begin
  Result.offset := offset;
  Result.length_ := length_;
  Result.indicator := indicator;
end;
function token_to_string(t: Token): string;
begin
  exit(((((('(' + IntToStr(t.offset)) + ', ') + IntToStr(t.length_)) + ', ') + t.indicator) + ')');
end;
function tokens_to_string(ts: TokenArray): string;
var
  tokens_to_string_res: string;
  tokens_to_string_i: integer;
begin
  tokens_to_string_res := '[';
  tokens_to_string_i := 0;
  while tokens_to_string_i < Length(ts) do begin
  tokens_to_string_res := tokens_to_string_res + token_to_string(ts[tokens_to_string_i]);
  if tokens_to_string_i < (Length(ts) - 1) then begin
  tokens_to_string_res := tokens_to_string_res + ', ';
end;
  tokens_to_string_i := tokens_to_string_i + 1;
end;
  exit(tokens_to_string_res + ']');
end;
function match_length_from_index(text: string; window: string; text_index: integer; window_index: integer): integer;
var
  match_length_from_index_tc: string;
  match_length_from_index_wc: string;
begin
  if (text_index >= Length(text)) or (window_index >= Length(window)) then begin
  exit(0);
end;
  match_length_from_index_tc := copy(text, text_index+1, (text_index + 1 - (text_index)));
  match_length_from_index_wc := copy(window, window_index+1, (window_index + 1 - (window_index)));
  if match_length_from_index_tc <> match_length_from_index_wc then begin
  exit(0);
end;
  exit(1 + match_length_from_index(text, window + match_length_from_index_tc, text_index + 1, window_index + 1));
end;
function find_encoding_token(text: string; search_buffer: string): Token;
var
  find_encoding_token_length_: integer;
  find_encoding_token_offset: integer;
  find_encoding_token_i: integer;
  find_encoding_token_ch: string;
  find_encoding_token_found_offset: integer;
  find_encoding_token_found_length: integer;
begin
  if Length(text) = 0 then begin
  panic('We need some text to work with.');
end;
  find_encoding_token_length_ := 0;
  find_encoding_token_offset := 0;
  if Length(search_buffer) = 0 then begin
  exit(makeToken(find_encoding_token_offset, find_encoding_token_length_, copy(text, 0+1, (1 - (0)))));
end;
  find_encoding_token_i := 0;
  while find_encoding_token_i < Length(search_buffer) do begin
  find_encoding_token_ch := copy(search_buffer, find_encoding_token_i+1, (find_encoding_token_i + 1 - (find_encoding_token_i)));
  find_encoding_token_found_offset := Length(search_buffer) - find_encoding_token_i;
  if find_encoding_token_ch = copy(text, 0+1, (1 - (0))) then begin
  find_encoding_token_found_length := match_length_from_index(text, search_buffer, 0, find_encoding_token_i);
  if find_encoding_token_found_length >= find_encoding_token_length_ then begin
  find_encoding_token_offset := find_encoding_token_found_offset;
  find_encoding_token_length_ := find_encoding_token_found_length;
end;
end;
  find_encoding_token_i := find_encoding_token_i + 1;
end;
  exit(makeToken(find_encoding_token_offset, find_encoding_token_length_, copy(text, find_encoding_token_length_+1, (find_encoding_token_length_ + 1 - (find_encoding_token_length_)))));
end;
function lz77_compress(text: string; window_size: integer; lookahead: integer): TokenArray;
var
  lz77_compress_search_buffer_size: integer;
  lz77_compress_output: array of Token;
  lz77_compress_search_buffer: string;
  lz77_compress_remaining: string;
  lz77_compress_token_var: Token;
  lz77_compress_add_len: integer;
begin
  lz77_compress_search_buffer_size := window_size - lookahead;
  lz77_compress_output := [];
  lz77_compress_search_buffer := '';
  lz77_compress_remaining := text;
  while Length(lz77_compress_remaining) > 0 do begin
  lz77_compress_token_var := find_encoding_token(lz77_compress_remaining, lz77_compress_search_buffer);
  lz77_compress_add_len := lz77_compress_token_var.length_ + 1;
  lz77_compress_search_buffer := lz77_compress_search_buffer + copy(lz77_compress_remaining, 0+1, (lz77_compress_add_len - (0)));
  if Length(lz77_compress_search_buffer) > lz77_compress_search_buffer_size then begin
  lz77_compress_search_buffer := copy(lz77_compress_search_buffer, Length(lz77_compress_search_buffer) - lz77_compress_search_buffer_size+1, (Length(lz77_compress_search_buffer) - (Length(lz77_compress_search_buffer) - lz77_compress_search_buffer_size)));
end;
  lz77_compress_remaining := copy(lz77_compress_remaining, lz77_compress_add_len+1, (Length(lz77_compress_remaining) - (lz77_compress_add_len)));
  lz77_compress_output := concat(lz77_compress_output, [lz77_compress_token_var]);
end;
  exit(lz77_compress_output);
end;
function lz77_decompress(tokens: TokenArray): string;
var
  lz77_decompress_output: string;
  lz77_decompress_t: Token;
  lz77_decompress_i: integer;
begin
  lz77_decompress_output := '';
  for lz77_decompress_t in tokens do begin
  lz77_decompress_i := 0;
  while lz77_decompress_i < lz77_decompress_t.length_ do begin
  lz77_decompress_output := lz77_decompress_output + copy(lz77_decompress_output, Length(lz77_decompress_output) - lz77_decompress_t.offset+1, ((Length(lz77_decompress_output) - lz77_decompress_t.offset) + 1 - (Length(lz77_decompress_output) - lz77_decompress_t.offset)));
  lz77_decompress_i := lz77_decompress_i + 1;
end;
  lz77_decompress_output := lz77_decompress_output + lz77_decompress_t.indicator;
end;
  exit(lz77_decompress_output);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  c1 := lz77_compress('ababcbababaa', 13, 6);
  writeln(tokens_to_string(c1));
  c2 := lz77_compress('aacaacabcabaaac', 13, 6);
  writeln(tokens_to_string(c2));
  tokens_example := [makeToken(0, 0, 'c'), makeToken(0, 0, 'a'), makeToken(0, 0, 'b'), makeToken(0, 0, 'r'), makeToken(3, 1, 'c'), makeToken(2, 1, 'd'), makeToken(7, 4, 'r'), makeToken(3, 5, 'd')];
  writeln(lz77_decompress(tokens_example));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
