{$mode objfpc}
program Main;
uses SysUtils, Variants;
type BoolArray = array of boolean;
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
function list_variant_to_str(xs: array of Variant): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + VarToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  used: BoolArray;
  sequence: array of Variant;
  sequence_2: array of Variant;
function repeat_bool(times: integer): BoolArray; forward;
function set_bool(xs: BoolArray; idx: integer; value: boolean): BoolArray; forward;
procedure create_state_space_tree(sequence: VariantArray; current: VariantArray; index: integer; used: BoolArray); forward;
procedure generate_all_permutations(sequence: VariantArray); forward;
function repeat_bool(times: integer): BoolArray;
var
  repeat_bool_res: array of boolean;
  repeat_bool_i: integer;
begin
  repeat_bool_res := [];
  repeat_bool_i := 0;
  while repeat_bool_i < times do begin
  repeat_bool_res := concat(repeat_bool_res, [false]);
  repeat_bool_i := repeat_bool_i + 1;
end;
  exit(repeat_bool_res);
end;
function set_bool(xs: BoolArray; idx: integer; value: boolean): BoolArray;
var
  set_bool_res: array of boolean;
  set_bool_i: integer;
begin
  set_bool_res := [];
  set_bool_i := 0;
  while set_bool_i < Length(xs) do begin
  if set_bool_i = idx then begin
  set_bool_res := concat(set_bool_res, [value]);
end else begin
  set_bool_res := concat(set_bool_res, [xs[set_bool_i]]);
end;
  set_bool_i := set_bool_i + 1;
end;
  exit(set_bool_res);
end;
procedure create_state_space_tree(sequence: VariantArray; current: VariantArray; index: integer; used: BoolArray);
var
  create_state_space_tree_i: integer;
  create_state_space_tree_next_current: array of Variant;
  create_state_space_tree_next_used: BoolArray;
begin
  if index = Length(sequence) then begin
  writeln(list_variant_to_str(current));
  exit();
end;
  create_state_space_tree_i := 0;
  while create_state_space_tree_i < Length(sequence) do begin
  if not used[create_state_space_tree_i] then begin
  create_state_space_tree_next_current := concat(current, [sequence[create_state_space_tree_i]]);
  create_state_space_tree_next_used := set_bool(used, create_state_space_tree_i, true);
  create_state_space_tree(sequence, create_state_space_tree_next_current, index + 1, create_state_space_tree_next_used);
end;
  create_state_space_tree_i := create_state_space_tree_i + 1;
end;
end;
procedure generate_all_permutations(sequence: VariantArray);
begin
  used := repeat_bool(Length(sequence));
  create_state_space_tree(sequence, VariantArray([]), 0, used);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  sequence := [3, 1, 2, 4];
  generate_all_permutations(sequence);
  sequence_2 := ['A', 'B', 'C'];
  generate_all_permutations(sequence_2);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
