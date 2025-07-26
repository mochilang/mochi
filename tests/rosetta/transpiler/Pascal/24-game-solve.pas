{$mode objfpc}
program Main;
uses SysUtils;
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
type Rational = record
  num: integer;
  denom: integer;
end;
type Node = record
  val: Rational;
  txt: string;
end;
type NodeArray = array of Node;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  OP_ADD: integer;
  OP_SUB: integer;
  OP_MUL: integer;
  OP_DIV: integer;
  combine_res: Rational;
  combine_opstr: string;
  n_cards: integer;
  goal: integer;
  digit_range: integer;
  solve_f: Rational;
  solve_i: integer;
  solve_j: integer;
  solve_rest: array of Node;
  solve_k: integer;
  solve_a: Node;
  solve_b: Node;
  solve_node_var: Node;
  op: integer;
  main_iter: integer;
  main_cards: array of Node;
  main_i: integer;
  main_n: integer;
function makeNode(val: Rational; txt: string): Node;
begin
  Result.val := val;
  Result.txt := txt;
end;
function makeRational(num: integer; denom: integer): Rational;
begin
  Result.num := num;
  Result.denom := denom;
end;
function makeNode(n: integer): Node;
begin
  exit(makeNode(makeRational(n, 1), IntToStr(n)));
end;
function combine(op: integer; l: Node; r: Node): Node;
begin
  if op = OP_ADD then begin
  combine_res := makeRational((l.val.num * r.val.denom) + (l.val.denom * r.val.num), l.val.denom * r.val.denom);
end else begin
  if op = OP_SUB then begin
  combine_res := makeRational((l.val.num * r.val.denom) - (l.val.denom * r.val.num), l.val.denom * r.val.denom);
end else begin
  if op = OP_MUL then begin
  combine_res := makeRational(l.val.num * r.val.num, l.val.denom * r.val.denom);
end else begin
  combine_res := makeRational(l.val.num * r.val.denom, l.val.denom * r.val.num);
end;
end;
end;
  combine_opstr := '';
  if op = OP_ADD then begin
  combine_opstr := ' + ';
end else begin
  if op = OP_SUB then begin
  combine_opstr := ' - ';
end else begin
  if op = OP_MUL then begin
  combine_opstr := ' * ';
end else begin
  combine_opstr := ' / ';
end;
end;
end;
  exit(makeNode(combine_res, ((('(' + l.txt) + combine_opstr) + r.txt) + ')'));
end;
function exprEval(x: Node): Rational;
begin
  exit(x.val);
end;
function exprString(x: Node): string;
begin
  exit(x.txt);
end;
function solve(xs: NodeArray): boolean;
begin
  if Length(xs) = 1 then begin
  solve_f := exprEval(xs[0]);
  if (solve_f.denom <> 0) and (solve_f.num = (solve_f.denom * goal)) then begin
  writeln(exprString(xs[0]));
  exit(true);
end;
  exit(false);
end;
  solve_i := 0;
  while solve_i < Length(xs) do begin
  solve_j := solve_i + 1;
  while solve_j < Length(xs) do begin
  solve_rest := [];
  solve_k := 0;
  while solve_k < Length(xs) do begin
  if (solve_k <> solve_i) and (solve_k <> solve_j) then begin
  solve_rest := concat(solve_rest, [xs[solve_k]]);
end;
  solve_k := solve_k + 1;
end;
  solve_a := xs[solve_i];
  solve_b := xs[solve_j];
  for op in [OP_ADD, OP_SUB, OP_MUL, OP_DIV] do begin
  solve_node_var := combine(op, solve_a, solve_b);
  if solve(concat(solve_rest, [solve_node_var])) then begin
  exit(true);
end;
end;
  solve_node_var := combine(OP_SUB, solve_b, solve_a);
  if solve(concat(solve_rest, [solve_node_var])) then begin
  exit(true);
end;
  solve_node_var := combine(OP_DIV, solve_b, solve_a);
  if solve(concat(solve_rest, [solve_node_var])) then begin
  exit(true);
end;
  solve_j := solve_j + 1;
end;
  solve_i := solve_i + 1;
end;
  exit(false);
end;
procedure main();
begin
  main_iter := 0;
  while main_iter < 10 do begin
  main_cards := [];
  main_i := 0;
  while main_i < n_cards do begin
  main_n := (_now() mod (digit_range - 1)) + 1;
  main_cards := concat(main_cards, [makeNode(main_n)]);
  writeln(' ' + IntToStr(main_n));
  main_i := main_i + 1;
end;
  writeln(':  ');
  if not solve(main_cards) then begin
  writeln('No solution');
end;
  main_iter := main_iter + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  OP_ADD := 1;
  OP_SUB := 2;
  OP_MUL := 3;
  OP_DIV := 4;
  n_cards := 4;
  goal := 24;
  digit_range := 9;
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
