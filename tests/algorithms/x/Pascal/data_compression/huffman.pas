{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type Huffman = record
  _tag: integer;
  Leaf_symbol: string;
  Leaf_freq: int64;
  Node_freq: int64;
  Node_left: Huffman;
  Node_right: Huffman;
end;
type IntArray = array of int64;
type StrArray = array of string;
type HuffmanArray = array of Huffman;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeHuffman(_tag: integer; Leaf_symbol: string; Leaf_freq: int64; Node_freq: int64; Node_left: Huffman; Node_right: Huffman): Huffman; forward;
function makeHuffman(freq: int64; left: Huffman; right: Huffman): Huffman; forward;
function makeHuffman(symbol: string; freq: int64): Huffman; forward;
function get_freq(get_freq_n: Huffman): int64; forward;
function sort_nodes(sort_nodes_nodes: HuffmanArray): HuffmanArray; forward;
function rest(rest_nodes: HuffmanArray): HuffmanArray; forward;
function count_freq(count_freq_text: string): HuffmanArray; forward;
function build_tree(build_tree_nodes: HuffmanArray): Huffman; forward;
function concat_pairs(concat_pairs_a: StrArrayArray; concat_pairs_b: StrArrayArray): StrArrayArray; forward;
function collect_codes(collect_codes_tree: Huffman; collect_codes_prefix: string): StrArrayArray; forward;
function find_code(find_code_pairs: StrArrayArray; find_code_ch: string): string; forward;
function huffman_encode(huffman_encode_text: string): string; forward;
function makeHuffman(_tag: integer; Leaf_symbol: string; Leaf_freq: int64; Node_freq: int64; Node_left: Huffman; Node_right: Huffman): Huffman;
begin
  Result._tag := _tag;
  Result.Leaf_symbol := Leaf_symbol;
  Result.Leaf_freq := Leaf_freq;
  Result.Node_freq := Node_freq;
  Result.Node_left := Node_left;
  Result.Node_right := Node_right;
end;
function makeHuffman(freq: int64; left: Huffman; right: Huffman): Huffman;
begin
  Result.Node_freq := freq;
  Result.Node_left := left;
  Result.Node_right := right;
  Result._tag := 1;
end;
function makeHuffman(symbol: string; freq: int64): Huffman;
begin
  Result.Leaf_symbol := symbol;
  Result.Leaf_freq := freq;
  Result._tag := 0;
end;
function get_freq(get_freq_n: Huffman): int64;
begin
  exit(IfThen(get_freq_n._tag = 0, get_freq_n.Leaf_freq, get_freq_n.Node_freq));
end;
function sort_nodes(sort_nodes_nodes: HuffmanArray): HuffmanArray;
var
  sort_nodes_arr: array of Huffman;
  sort_nodes_i: int64;
  sort_nodes_key: Huffman;
  sort_nodes_j: int64;
begin
  sort_nodes_arr := sort_nodes_nodes;
  sort_nodes_i := 1;
  while sort_nodes_i < Length(sort_nodes_arr) do begin
  sort_nodes_key := sort_nodes_arr[sort_nodes_i];
  sort_nodes_j := sort_nodes_i - 1;
  while (sort_nodes_j >= 0) and (get_freq(sort_nodes_arr[sort_nodes_j]) > get_freq(sort_nodes_key)) do begin
  sort_nodes_arr[sort_nodes_j + 1] := sort_nodes_arr[sort_nodes_j];
  sort_nodes_j := sort_nodes_j - 1;
end;
  sort_nodes_arr[sort_nodes_j + 1] := sort_nodes_key;
  sort_nodes_i := sort_nodes_i + 1;
end;
  exit(sort_nodes_arr);
end;
function rest(rest_nodes: HuffmanArray): HuffmanArray;
var
  rest_res: array of Huffman;
  rest_i: int64;
begin
  rest_res := [];
  rest_i := 1;
  while rest_i < Length(rest_nodes) do begin
  rest_res := concat(rest_res, [rest_nodes[rest_i]]);
  rest_i := rest_i + 1;
end;
  exit(rest_res);
end;
function count_freq(count_freq_text: string): HuffmanArray;
var
  count_freq_chars: array of string;
  count_freq_freqs: array of int64;
  count_freq_i: int64;
  count_freq_c: string;
  count_freq_j: int64;
  count_freq_found: boolean;
  count_freq_leaves: array of Huffman;
  count_freq_k: int64;
begin
  count_freq_chars := [];
  count_freq_freqs := [];
  count_freq_i := 0;
  while count_freq_i < Length(count_freq_text) do begin
  count_freq_c := copy(count_freq_text, count_freq_i+1, (count_freq_i + 1 - (count_freq_i)));
  count_freq_j := 0;
  count_freq_found := false;
  while count_freq_j < Length(count_freq_chars) do begin
  if count_freq_chars[count_freq_j] = count_freq_c then begin
  count_freq_freqs[count_freq_j] := count_freq_freqs[count_freq_j] + 1;
  count_freq_found := true;
  break;
end;
  count_freq_j := count_freq_j + 1;
end;
  if not count_freq_found then begin
  count_freq_chars := concat(count_freq_chars, StrArray([count_freq_c]));
  count_freq_freqs := concat(count_freq_freqs, IntArray([1]));
end;
  count_freq_i := count_freq_i + 1;
end;
  count_freq_leaves := [];
  count_freq_k := 0;
  while count_freq_k < Length(count_freq_chars) do begin
  count_freq_leaves := concat(count_freq_leaves, [makecount_freq_Leaf(count_freq_chars[count_freq_k], count_freq_freqs[count_freq_k])]);
  count_freq_k := count_freq_k + 1;
end;
  exit(sort_nodes(count_freq_leaves));
end;
function build_tree(build_tree_nodes: HuffmanArray): Huffman;
var
  build_tree_arr: array of Huffman;
  build_tree_left: Huffman;
  build_tree_right: Huffman;
  build_tree_node: build_tree_Node_21;
begin
  build_tree_arr := build_tree_nodes;
  while Length(build_tree_arr) > 1 do begin
  build_tree_left := build_tree_arr[0];
  build_tree_arr := rest(build_tree_arr);
  build_tree_right := build_tree_arr[0];
  build_tree_arr := rest(build_tree_arr);
  build_tree_node := makebuild_tree_Node_21(get_freq(build_tree_left) + get_freq(build_tree_right), build_tree_left, build_tree_right);
  build_tree_arr := concat(build_tree_arr, [build_tree_node]);
  build_tree_arr := sort_nodes(build_tree_arr);
end;
  exit(build_tree_arr[0]);
end;
function concat_pairs(concat_pairs_a: StrArrayArray; concat_pairs_b: StrArrayArray): StrArrayArray;
var
  concat_pairs_res: array of StrArray;
  concat_pairs_i: int64;
begin
  concat_pairs_res := concat_pairs_a;
  concat_pairs_i := 0;
  while concat_pairs_i < Length(concat_pairs_b) do begin
  concat_pairs_res := concat(concat_pairs_res, [concat_pairs_b[concat_pairs_i]]);
  concat_pairs_i := concat_pairs_i + 1;
end;
  exit(concat_pairs_res);
end;
function collect_codes(collect_codes_tree: Huffman; collect_codes_prefix: string): StrArrayArray;
begin
  exit(IfThen(collect_codes_tree._tag = 0, [[s, collect_codes_prefix]], concat_pairs(collect_codes(collect_codes_tree.Node_left, collect_codes_prefix + '0'), collect_codes(collect_codes_tree.Node_right, collect_codes_prefix + '1'))));
end;
function find_code(find_code_pairs: StrArrayArray; find_code_ch: string): string;
var
  find_code_i: int64;
begin
  find_code_i := 0;
  while find_code_i < Length(find_code_pairs) do begin
  if find_code_pairs[find_code_i][0] = find_code_ch then begin
  exit(find_code_pairs[find_code_i][1]);
end;
  find_code_i := find_code_i + 1;
end;
  exit('');
end;
function huffman_encode(huffman_encode_text: string): string;
var
  huffman_encode_leaves: HuffmanArray;
  huffman_encode_tree: Huffman;
  huffman_encode_codes: StrArrayArray;
  huffman_encode_encoded: string;
  huffman_encode_i: int64;
  huffman_encode_c: string;
begin
  if huffman_encode_text = '' then begin
  exit('');
end;
  huffman_encode_leaves := count_freq(huffman_encode_text);
  huffman_encode_tree := build_tree(huffman_encode_leaves);
  huffman_encode_codes := collect_codes(huffman_encode_tree, '');
  huffman_encode_encoded := '';
  huffman_encode_i := 0;
  while huffman_encode_i < Length(huffman_encode_text) do begin
  huffman_encode_c := copy(huffman_encode_text, huffman_encode_i+1, (huffman_encode_i + 1 - (huffman_encode_i)));
  huffman_encode_encoded := (huffman_encode_encoded + find_code(huffman_encode_codes, huffman_encode_c)) + ' ';
  huffman_encode_i := huffman_encode_i + 1;
end;
  exit(huffman_encode_encoded);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(huffman_encode('beep boop beer!'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
