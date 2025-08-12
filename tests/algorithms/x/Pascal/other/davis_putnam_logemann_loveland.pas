{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils, fgl;
type Clause = record
  literals: specialize TFPGMap<string, integer>;
  names: array of string;
end;
type EvalResult = record
  value: integer;
  clause: Clause;
end;
type Formula = record
  clauses: array of Clause;
end;
type DPLLResult = record
  sat: boolean;
  model: specialize TFPGMap<string, integer>;
end;
type StrArray = array of string;
type ClauseArray = array of Clause;
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
  clause1: Clause;
  clause2: Clause;
  formula_var: Formula;
  formula_str: string;
  clauses: array of Clause;
  symbols: array of string;
  model: specialize TFPGMap<string, integer>;
  result_: DPLLResult;
  c: Clause;
  f: Formula;
  s: string;
  lits: StrArray;
  cs: ClauseArray;
function Map1(): specialize TFPGMap<string, integer>; forward;
function makeDPLLResult(sat: boolean; model: specialize TFPGMap<string, integer>): DPLLResult; forward;
function makeFormula(clauses: ClauseArray): Formula; forward;
function makeEvalResult(value: integer; clause: Clause): EvalResult; forward;
function makeClause(literals: specialize TFPGMap<string, integer>; names: StrArray): Clause; forward;
function new_clause(lits: StrArray): Clause; forward;
function assign_clause(c: Clause; model: specialize TFPGMap<string, integer>): Clause; forward;
function evaluate_clause(c: Clause; model: specialize TFPGMap<string, integer>): EvalResult; forward;
function new_formula(cs: ClauseArray): Formula; forward;
function remove_symbol(symbols: StrArray; s: string): StrArray; forward;
function dpll_algorithm(clauses: ClauseArray; symbols: StrArray; model: specialize TFPGMap<string, integer>): DPLLResult; forward;
function str_clause(c: Clause): string; forward;
function str_formula(f: Formula): string; forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
end;
function makeDPLLResult(sat: boolean; model: specialize TFPGMap<string, integer>): DPLLResult;
begin
  Result.sat := sat;
  Result.model := model;
end;
function makeFormula(clauses: ClauseArray): Formula;
begin
  Result.clauses := clauses;
end;
function makeEvalResult(value: integer; clause: Clause): EvalResult;
begin
  Result.value := value;
  Result.clause := clause;
end;
function makeClause(literals: specialize TFPGMap<string, integer>; names: StrArray): Clause;
begin
  Result.literals := literals;
  Result.names := names;
end;
function new_clause(lits: StrArray): Clause;
var
  new_clause_m: specialize TFPGMap<string, integer>;
  new_clause_names: array of string;
  new_clause_i: integer;
  new_clause_lit: string;
begin
  new_clause_m := specialize TFPGMap<string, integer>.Create();
  new_clause_names := [];
  new_clause_i := 0;
  while new_clause_i < Length(lits) do begin
  new_clause_lit := lits[new_clause_i];
  new_clause_m[new_clause_lit] := 0 - 1;
  new_clause_names := concat(new_clause_names, StrArray([new_clause_lit]));
  new_clause_i := new_clause_i + 1;
end;
  exit(makeClause(new_clause_m, new_clause_names));
end;
function assign_clause(c: Clause; model: specialize TFPGMap<string, integer>): Clause;
var
  assign_clause_lits: specialize TFPGMap<string, integer>;
  assign_clause_i: integer;
  assign_clause_lit: string;
  assign_clause_symbol: string;
  assign_clause_value: integer;
begin
  assign_clause_lits := c.literals;
  assign_clause_i := 0;
  while assign_clause_i < Length(c.names) do begin
  assign_clause_lit := c.names[assign_clause_i];
  assign_clause_symbol := copy(assign_clause_lit, 1, 2);
  if model.IndexOf(assign_clause_symbol) <> -1 then begin
  assign_clause_value := model[assign_clause_symbol];
  if (copy(assign_clause_lit, Length(assign_clause_lit) - 1+1, (Length(assign_clause_lit) - (Length(assign_clause_lit) - 1))) = '''') and (assign_clause_value <> (0 - 1)) then begin
  assign_clause_value := 1 - assign_clause_value;
end;
  assign_clause_lits[assign_clause_lit] := assign_clause_value;
end;
  assign_clause_i := assign_clause_i + 1;
end;
  c.literals := assign_clause_lits;
  exit(c);
end;
function evaluate_clause(c: Clause; model: specialize TFPGMap<string, integer>): EvalResult;
var
  evaluate_clause_i: integer;
  evaluate_clause_lit: string;
  evaluate_clause_sym: string;
  evaluate_clause_value: integer;
  evaluate_clause_value_idx: integer;
  evaluate_clause_any_true: integer;
begin
  evaluate_clause_i := 0;
  while evaluate_clause_i < Length(c.names) do begin
  evaluate_clause_lit := c.names[evaluate_clause_i];
  if copy(evaluate_clause_lit, Length(evaluate_clause_lit) - 1+1, (Length(evaluate_clause_lit) - (Length(evaluate_clause_lit) - 1))) = '''' then begin
  evaluate_clause_sym := copy(evaluate_clause_lit, 1, 2);
end else begin
  evaluate_clause_sym := evaluate_clause_lit + '''';
end;
  if c.literals.IndexOf(evaluate_clause_sym) <> -1 then begin
  exit(makeEvalResult(1, c));
end;
  evaluate_clause_i := evaluate_clause_i + 1;
end;
  c := assign_clause(c, model);
  evaluate_clause_i := 0;
  while evaluate_clause_i < Length(c.names) do begin
  evaluate_clause_lit := c.names[evaluate_clause_i];
  evaluate_clause_value_idx := c.literals.IndexOf(evaluate_clause_lit);
  if evaluate_clause_value_idx <> -1 then begin
  evaluate_clause_value := c.literals.Data[evaluate_clause_value_idx];
end else begin
  evaluate_clause_value := 0;
end;
  if evaluate_clause_value = 1 then begin
  exit(makeEvalResult(1, c));
end;
  if evaluate_clause_value = (0 - 1) then begin
  exit(makeEvalResult(0 - 1, c));
end;
  evaluate_clause_i := evaluate_clause_i + 1;
end;
  evaluate_clause_any_true := 0;
  evaluate_clause_i := 0;
  while evaluate_clause_i < Length(c.names) do begin
  evaluate_clause_lit := c.names[evaluate_clause_i];
  if c.literals[evaluate_clause_lit] = 1 then begin
  evaluate_clause_any_true := 1;
end;
  evaluate_clause_i := evaluate_clause_i + 1;
end;
  exit(makeEvalResult(evaluate_clause_any_true, c));
end;
function new_formula(cs: ClauseArray): Formula;
begin
  exit(makeFormula(cs));
end;
function remove_symbol(symbols: StrArray; s: string): StrArray;
var
  remove_symbol_res: array of string;
  remove_symbol_i: integer;
begin
  remove_symbol_res := [];
  remove_symbol_i := 0;
  while remove_symbol_i < Length(symbols) do begin
  if symbols[remove_symbol_i] <> s then begin
  remove_symbol_res := concat(remove_symbol_res, StrArray([symbols[remove_symbol_i]]));
end;
  remove_symbol_i := remove_symbol_i + 1;
end;
  exit(remove_symbol_res);
end;
function dpll_algorithm(clauses: ClauseArray; symbols: StrArray; model: specialize TFPGMap<string, integer>): DPLLResult;
var
  dpll_algorithm_all_true: boolean;
  dpll_algorithm_i: integer;
  dpll_algorithm_ev: EvalResult;
  dpll_algorithm_p: string;
  dpll_algorithm_rest: StrArray;
  dpll_algorithm_tmp1: specialize TFPGMap<string, integer>;
  dpll_algorithm_tmp2: specialize TFPGMap<string, integer>;
  dpll_algorithm_res1: DPLLResult;
begin
  dpll_algorithm_all_true := true;
  dpll_algorithm_i := 0;
  while dpll_algorithm_i < Length(clauses) do begin
  dpll_algorithm_ev := evaluate_clause(clauses[dpll_algorithm_i], model);
  clauses[dpll_algorithm_i] := dpll_algorithm_ev.clause;
  if dpll_algorithm_ev.value = 0 then begin
  exit(makeDPLLResult(false, Map1()));
end else begin
  if dpll_algorithm_ev.value = (0 - 1) then begin
  dpll_algorithm_all_true := false;
end;
end;
  dpll_algorithm_i := dpll_algorithm_i + 1;
end;
  if dpll_algorithm_all_true then begin
  exit(makeDPLLResult(true, model));
end;
  dpll_algorithm_p := symbols[0];
  dpll_algorithm_rest := remove_symbol(symbols, dpll_algorithm_p);
  dpll_algorithm_tmp1 := model;
  dpll_algorithm_tmp2 := model;
  dpll_algorithm_tmp1[dpll_algorithm_p] := 1;
  dpll_algorithm_tmp2[dpll_algorithm_p] := 0;
  dpll_algorithm_res1 := dpll_algorithm(clauses, dpll_algorithm_rest, dpll_algorithm_tmp1);
  if dpll_algorithm_res1.sat then begin
  exit(dpll_algorithm_res1);
end;
  exit(dpll_algorithm(clauses, dpll_algorithm_rest, dpll_algorithm_tmp2));
end;
function str_clause(c: Clause): string;
var
  str_clause_line: string;
  str_clause_first: boolean;
  str_clause_i: integer;
  str_clause_lit: string;
begin
  str_clause_line := '{';
  str_clause_first := true;
  str_clause_i := 0;
  while str_clause_i < Length(c.names) do begin
  str_clause_lit := c.names[str_clause_i];
  if str_clause_first then begin
  str_clause_first := false;
end else begin
  str_clause_line := str_clause_line + ' , ';
end;
  str_clause_line := str_clause_line + str_clause_lit;
  str_clause_i := str_clause_i + 1;
end;
  str_clause_line := str_clause_line + '}';
  exit(str_clause_line);
end;
function str_formula(f: Formula): string;
var
  str_formula_line: string;
  str_formula_i: integer;
begin
  str_formula_line := '{';
  str_formula_i := 0;
  while str_formula_i < Length(f.clauses) do begin
  str_formula_line := str_formula_line + str_clause(f.clauses[str_formula_i]);
  if str_formula_i < (Length(f.clauses) - 1) then begin
  str_formula_line := str_formula_line + ' , ';
end;
  str_formula_i := str_formula_i + 1;
end;
  str_formula_line := str_formula_line + '}';
  exit(str_formula_line);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  clause1 := new_clause(['A4', 'A3', 'A5''', 'A1', 'A3''']);
  clause2 := new_clause(['A4']);
  formula_var := new_formula([clause1, clause2]);
  formula_str := str_formula(formula_var);
  clauses := [clause1, clause2];
  symbols := ['A4', 'A3', 'A5', 'A1'];
  model := specialize TFPGMap<string, integer>.Create();
  result_ := dpll_algorithm(clauses, symbols, model);
  if result_.sat then begin
  writeln(('The formula ' + formula_str) + ' is satisfiable.');
end else begin
  writeln(('The formula ' + formula_str) + ' is not satisfiable.');
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
