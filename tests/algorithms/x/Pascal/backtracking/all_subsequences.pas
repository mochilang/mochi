{$mode objfpc}
program Main;
uses SysUtils, Variants;
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
  seq: array of Variant;
  seq2: array of Variant;
procedure create_state_space_tree(sequence: VariantArray; current: VariantArray; index: integer); forward;
procedure generate_all_subsequences(sequence: VariantArray); forward;
procedure create_state_space_tree(sequence: VariantArray; current: VariantArray; index: integer);
var
  create_state_space_tree_with_elem: array of Variant;
begin
  if index = Length(sequence) then begin
  writeln(list_variant_to_str(current));
  exit();
end;
  create_state_space_tree(sequence, current, index + 1);
  create_state_space_tree_with_elem := concat(current, [sequence[index]]);
  create_state_space_tree(sequence, create_state_space_tree_with_elem, index + 1);
end;
procedure generate_all_subsequences(sequence: VariantArray);
begin
  create_state_space_tree(sequence, VariantArray([]), 0);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seq := [1, 2, 3];
  generate_all_subsequences(seq);
  seq2 := ['A', 'B', 'C'];
  generate_all_subsequences(seq2);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
