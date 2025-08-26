{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils, fgl;
type Clause = record
  literals: specialize TFPGMap<string, int64>;
  names: array of string;
end;
type DPLLResult = record
  sat: boolean;
  model: specialize TFPGMap<string, int64>;
end;
type StrArray = array of string;
type ClauseArray = array of Clause;
type EvalResult = record
  value: int64;
  clause: Clause;
end;
type Formula = record
  clauses: array of Clause;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  clause1: Clause;
  clause2: Clause;
  formula_37_var: Formula;
  formula_str: string;
  clauses: array of Clause;
  symbols: array of string;
  model: specialize TFPGMap<string, int64>;
  result_: DPLLResult;
function Map1(): specialize TFPGMap<string, int64>; forward;
function makeDPLLResult(sat: boolean; model: specialize TFPGMap<string, int64>): DPLLResult; forward;
function makeFormula(clauses: ClauseArray): Formula; forward;
function makeEvalResult(value: int64; clause_56_var: Clause): EvalResult; forward;
function makeClause(literals: specialize TFPGMap<string, int64>; names: StrArray): Clause; forward;
function new_clause(new_clause_lits: StrArray): Clause; forward;
function assign_clause(assign_clause_c: Clause; assign_clause_model: specialize TFPGMap<string, int64>): Clause; forward;
function evaluate_clause(evaluate_clause_c: Clause; evaluate_clause_model: specialize TFPGMap<string, int64>): EvalResult; forward;
function new_formula(new_formula_cs: ClauseArray): Formula; forward;
function remove_symbol(remove_symbol_symbols: StrArray; remove_symbol_s: string): StrArray; forward;
function dpll_algorithm(dpll_algorithm_clauses: ClauseArray; dpll_algorithm_symbols: StrArray; dpll_algorithm_model: specialize TFPGMap<string, int64>): DPLLResult; forward;
function str_clause(str_clause_c: Clause): string; forward;
function str_formula(str_formula_f: Formula): string; forward;
function Map1(): specialize TFPGMap<string, int64>;
begin
  Result := specialize TFPGMap<string, int64>.Create();
end;
function makeDPLLResult(sat: boolean; model: specialize TFPGMap<string, int64>): DPLLResult;
begin
  Result.sat := sat;
  Result.model := model;
end;
function makeFormula(clauses: ClauseArray): Formula;
begin
  Result.clauses := clauses;
end;
function makeEvalResult(value: int64; clause_56_var: Clause): EvalResult;
begin
  Result.value := value;
  Result.clause := clause_56_var;
end;
function makeClause(literals: specialize TFPGMap<string, int64>; names: StrArray): Clause;
begin
  Result.literals := literals;
  Result.names := names;
end;
function new_clause(new_clause_lits: StrArray): Clause;
var
  new_clause_m: specialize TFPGMap<string, int64>;
  new_clause_names: array of string;
  new_clause_i: int64;
  new_clause_lit: string;
begin
  new_clause_m := specialize TFPGMap<string, int64>.Create();
  new_clause_names := [];
  new_clause_i := 0;
  while new_clause_i < Length(new_clause_lits) do begin
  new_clause_lit := new_clause_lits[new_clause_i];
  new_clause_m[new_clause_lit] := 0 - 1;
  new_clause_names := concat(new_clause_names, StrArray([new_clause_lit]));
  new_clause_i := new_clause_i + 1;
end;
  exit(makeClause(new_clause_m, new_clause_names));
end;
function assign_clause(assign_clause_c: Clause; assign_clause_model: specialize TFPGMap<string, int64>): Clause;
var
  assign_clause_lits: specialize TFPGMap<string, int64>;
  assign_clause_i: int64;
  assign_clause_lit: string;
  assign_clause_symbol: string;
  assign_clause_value: int64;
begin
  assign_clause_lits := assign_clause_c.literals;
  assign_clause_i := 0;
  while assign_clause_i < Length(assign_clause_c.names) do begin
  assign_clause_lit := assign_clause_c.names[assign_clause_i];
  assign_clause_symbol := copy(assign_clause_lit, 1, 2);
  if assign_clause_model.IndexOf(assign_clause_symbol) <> -1 then begin
  assign_clause_value := assign_clause_model[assign_clause_symbol];
  if (copy(assign_clause_lit, Length(assign_clause_lit) - 1+1, (Length(assign_clause_lit) - (Length(assign_clause_lit) - 1))) = '''') and (assign_clause_value <> (0 - 1)) then begin
  assign_clause_value := 1 - assign_clause_value;
end;
  assign_clause_lits[assign_clause_lit] := assign_clause_value;
end;
  assign_clause_i := assign_clause_i + 1;
end;
  assign_clause_c.literals := assign_clause_lits;
  exit(assign_clause_c);
end;
function evaluate_clause(evaluate_clause_c: Clause; evaluate_clause_model: specialize TFPGMap<string, int64>): EvalResult;
var
  evaluate_clause_i: int64;
  evaluate_clause_lit: string;
  evaluate_clause_sym: string;
  evaluate_clause_value: int64;
  evaluate_clause_value_idx: integer;
  evaluate_clause_any_true: int64;
begin
  evaluate_clause_i := 0;
  while evaluate_clause_i < Length(evaluate_clause_c.names) do begin
  evaluate_clause_lit := evaluate_clause_c.names[evaluate_clause_i];
  if copy(evaluate_clause_lit, Length(evaluate_clause_lit) - 1+1, (Length(evaluate_clause_lit) - (Length(evaluate_clause_lit) - 1))) = '''' then begin
  evaluate_clause_sym := copy(evaluate_clause_lit, 1, 2);
end else begin
  evaluate_clause_sym := evaluate_clause_lit + '''';
end;
  if evaluate_clause_c.literals.IndexOf(evaluate_clause_sym) <> -1 then begin
  exit(makeEvalResult(1, evaluate_clause_c));
end;
  evaluate_clause_i := evaluate_clause_i + 1;
end;
  evaluate_clause_c := assign_clause(evaluate_clause_c, evaluate_clause_model);
  evaluate_clause_i := 0;
  while evaluate_clause_i < Length(evaluate_clause_c.names) do begin
  evaluate_clause_lit := evaluate_clause_c.names[evaluate_clause_i];
  evaluate_clause_value_idx := evaluate_clause_c.literals.IndexOf(evaluate_clause_lit);
  if evaluate_clause_value_idx <> -1 then begin
  evaluate_clause_value := evaluate_clause_c.literals.Data[evaluate_clause_value_idx];
end else begin
  evaluate_clause_value := 0;
end;
  if evaluate_clause_value = 1 then begin
  exit(makeEvalResult(1, evaluate_clause_c));
end;
  if evaluate_clause_value = (0 - 1) then begin
  exit(makeEvalResult(0 - 1, evaluate_clause_c));
end;
  evaluate_clause_i := evaluate_clause_i + 1;
end;
  evaluate_clause_any_true := 0;
  evaluate_clause_i := 0;
  while evaluate_clause_i < Length(evaluate_clause_c.names) do begin
  evaluate_clause_lit := evaluate_clause_c.names[evaluate_clause_i];
  if evaluate_clause_c.literals[evaluate_clause_lit] = 1 then begin
  evaluate_clause_any_true := 1;
end;
  evaluate_clause_i := evaluate_clause_i + 1;
end;
  exit(makeEvalResult(evaluate_clause_any_true, evaluate_clause_c));
end;
function new_formula(new_formula_cs: ClauseArray): Formula;
begin
  exit(makeFormula(new_formula_cs));
end;
function remove_symbol(remove_symbol_symbols: StrArray; remove_symbol_s: string): StrArray;
var
  remove_symbol_res: array of string;
  remove_symbol_i: int64;
begin
  remove_symbol_res := [];
  remove_symbol_i := 0;
  while remove_symbol_i < Length(remove_symbol_symbols) do begin
  if remove_symbol_symbols[remove_symbol_i] <> remove_symbol_s then begin
  remove_symbol_res := concat(remove_symbol_res, StrArray([remove_symbol_symbols[remove_symbol_i]]));
end;
  remove_symbol_i := remove_symbol_i + 1;
end;
  exit(remove_symbol_res);
end;
function dpll_algorithm(dpll_algorithm_clauses: ClauseArray; dpll_algorithm_symbols: StrArray; dpll_algorithm_model: specialize TFPGMap<string, int64>): DPLLResult;
var
  dpll_algorithm_all_true: boolean;
  dpll_algorithm_i: int64;
  dpll_algorithm_ev: EvalResult;
  dpll_algorithm_p: string;
  dpll_algorithm_rest: StrArray;
  dpll_algorithm_tmp1: specialize TFPGMap<string, int64>;
  dpll_algorithm_tmp2: specialize TFPGMap<string, int64>;
  dpll_algorithm_res1: DPLLResult;
begin
  dpll_algorithm_all_true := true;
  dpll_algorithm_i := 0;
  while dpll_algorithm_i < Length(dpll_algorithm_clauses) do begin
  dpll_algorithm_ev := evaluate_clause(dpll_algorithm_clauses[dpll_algorithm_i], dpll_algorithm_model);
  dpll_algorithm_clauses[dpll_algorithm_i] := dpll_algorithm_ev.clause;
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
  exit(makeDPLLResult(true, dpll_algorithm_model));
end;
  dpll_algorithm_p := dpll_algorithm_symbols[0];
  dpll_algorithm_rest := remove_symbol(dpll_algorithm_symbols, dpll_algorithm_p);
  dpll_algorithm_tmp1 := dpll_algorithm_model;
  dpll_algorithm_tmp2 := dpll_algorithm_model;
  dpll_algorithm_tmp1[dpll_algorithm_p] := 1;
  dpll_algorithm_tmp2[dpll_algorithm_p] := 0;
  dpll_algorithm_res1 := dpll_algorithm(dpll_algorithm_clauses, dpll_algorithm_rest, dpll_algorithm_tmp1);
  if dpll_algorithm_res1.sat then begin
  exit(dpll_algorithm_res1);
end;
  exit(dpll_algorithm(dpll_algorithm_clauses, dpll_algorithm_rest, dpll_algorithm_tmp2));
end;
function str_clause(str_clause_c: Clause): string;
var
  str_clause_line: string;
  str_clause_first: boolean;
  str_clause_i: int64;
  str_clause_lit: string;
begin
  str_clause_line := '{';
  str_clause_first := true;
  str_clause_i := 0;
  while str_clause_i < Length(str_clause_c.names) do begin
  str_clause_lit := str_clause_c.names[str_clause_i];
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
function str_formula(str_formula_f: Formula): string;
var
  str_formula_line: string;
  str_formula_i: int64;
begin
  str_formula_line := '{';
  str_formula_i := 0;
  while str_formula_i < Length(str_formula_f.clauses) do begin
  str_formula_line := str_formula_line + str_clause(str_formula_f.clauses[str_formula_i]);
  if str_formula_i < (Length(str_formula_f.clauses) - 1) then begin
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
  formula_37_var := new_formula([clause1, clause2]);
  formula_str := str_formula(formula_37_var);
  clauses := [clause1, clause2];
  symbols := ['A4', 'A3', 'A5', 'A1'];
  model := specialize TFPGMap<string, int64>.Create();
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
  writeln('');
end.
