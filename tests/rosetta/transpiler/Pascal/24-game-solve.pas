{$mode objfpc}
program Main;
uses SysUtils;
type ExprArray = array of Expr;
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
type Rational = record
  num: integer;
  denom: integer;
end;
type Expr = record
end;
var
  OP_ADD: integer;
  OP_SUB: integer;
  OP_MUL: integer;
  OP_DIV: integer;
  binEval_lv: integer;
  binEval_rv: integer;
  binString_ls: integer;
  binString_rs: integer;
  binString_opstr: string;
  n_cards: integer;
  goal: integer;
  digit_range: integer;
  solve_f: Rational;
  solve_i: integer;
  solve_j: integer;
  solve_rest: array of Expr;
  solve_k: integer;
  solve_a: Expr;
  solve_b: Expr;
  solve_node: integer;
  op: integer;
  main_iter: integer;
  main_cards: array of Expr;
  main_i: integer;
  main_n: integer;
function makeExpr(): Expr;
begin
end;
function makeRational(num: integer; denom: integer): Rational;
begin
  Result.num := num;
  Result.denom := denom;
end;
function binEval(op: integer; l: Expr; r: Expr): Rational;
begin
  binEval_lv := exprEval(l);
  binEval_rv := exprEval(r);
  if op = OP_ADD then begin
  exit(makeRational((binEval_lv.num * binEval_rv.denom) + (binEval_lv.denom * binEval_rv.num), binEval_lv.denom * binEval_rv.denom));
end;
  if op = OP_SUB then begin
  exit(makeRational((binEval_lv.num * binEval_rv.denom) - (binEval_lv.denom * binEval_rv.num), binEval_lv.denom * binEval_rv.denom));
end;
  if op = OP_MUL then begin
  exit(makeRational(binEval_lv.num * binEval_rv.num, binEval_lv.denom * binEval_rv.denom));
end;
  exit(makeRational(binEval_lv.num * binEval_rv.denom, binEval_lv.denom * binEval_rv.num));
end;
function binString(op: integer; l: Expr; r: Expr): string;
begin
  binString_ls := exprString(l);
  binString_rs := exprString(r);
  binString_opstr := '';
  if op = OP_ADD then begin
  binString_opstr := ' + ';
end else begin
  if op = OP_SUB then begin
  binString_opstr := ' - ';
end else begin
  if op = OP_MUL then begin
  binString_opstr := ' * ';
end else begin
  binString_opstr := ' / ';
end;
end;
end;
  exit(((('(' + binString_ls) + binString_opstr) + binString_rs) + ')');
end;
function newNum(n: integer): Expr;
begin
  exit(makeNum(makeRational(n, 1)));
end;
function exprEval(x: Expr): Rational;
begin
  exit(IfThen(x = Num(v), v, binEval(op, l, r)));
end;
function exprString(x: Expr): string;
begin
  exit(IfThen(x = Num(v), IntToStr(v.num), binString(op, l, r)));
end;
function solve(xs: ExprArray): boolean;
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
  solve_node := makeBin(OP_ADD, solve_a, solve_b);
  for op in [OP_ADD, OP_SUB, OP_MUL, OP_DIV] do begin
  solve_node := makeBin(op, solve_a, solve_b);
  if solve(concat(solve_rest, [solve_node])) then begin
  exit(true);
end;
end;
  solve_node := makeBin(OP_SUB, solve_b, solve_a);
  if solve(concat(solve_rest, [solve_node])) then begin
  exit(true);
end;
  solve_node := makeBin(OP_DIV, solve_b, solve_a);
  if solve(concat(solve_rest, [solve_node])) then begin
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
  main_cards := concat(main_cards, [newNum(main_n)]);
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
  OP_ADD := 1;
  OP_SUB := 2;
  OP_MUL := 3;
  OP_DIV := 4;
  n_cards := 4;
  goal := 24;
  digit_range := 9;
  main();
end.
