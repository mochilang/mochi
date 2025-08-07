<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function new_clause($lits) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $symbols, $model, $result;
  $m = [];
  $names = [];
  $i = 0;
  while ($i < count($lits)) {
  $lit = $lits[$i];
  $m[$lit] = 0 - 1;
  $names = _append($names, $lit);
  $i = $i + 1;
};
  return ['literals' => $m, 'names' => $names];
};
  function assign_clause(&$c, $model) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $symbols, $result;
  $lits = $c['literals'];
  $i = 0;
  while ($i < _len($c['names'])) {
  $lit = $c['names'][$i];
  $symbol = substr($lit, 0, 2 - 0);
  if (array_key_exists($symbol, $model)) {
  $value = $model[$symbol];
  if (substr($lit, strlen($lit) - 1, strlen($lit) - (strlen($lit) - 1)) == '\'' && $value != 0 - 1) {
  $value = 1 - $value;
};
  $lits[$lit] = $value;
}
  $i = $i + 1;
};
  $c['literals'] = $lits;
  return $c;
};
  function evaluate_clause($c, $model) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $symbols, $result;
  $i = 0;
  while ($i < _len($c['names'])) {
  $lit = $c['names'][$i];
  $sym = (substr($lit, strlen($lit) - 1, strlen($lit) - (strlen($lit) - 1)) == '\'' ? substr($lit, 0, 2 - 0) : $lit . '\'');
  if (isset($c['literals'][$sym])) {
  return ['value' => 1, 'clause' => $c];
}
  $i = $i + 1;
};
  $c = assign_clause($c, $model);
  $i = 0;
  while ($i < _len($c['names'])) {
  $lit = $c['names'][$i];
  $value = $c['literals'][$lit];
  if ($value == 1) {
  return ['value' => 1, 'clause' => $c];
}
  if ($value == 0 - 1) {
  return ['value' => 0 - 1, 'clause' => $c];
}
  $i = $i + 1;
};
  $any_true = 0;
  $i = 0;
  while ($i < _len($c['names'])) {
  $lit = $c['names'][$i];
  if ($c['literals'][$lit] == 1) {
  $any_true = 1;
}
  $i = $i + 1;
};
  return ['value' => $any_true, 'clause' => $c];
};
  function new_formula($cs) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $symbols, $model, $result;
  return ['clauses' => $cs];
};
  function remove_symbol($symbols, $s) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $model, $result;
  $res = [];
  $i = 0;
  while ($i < count($symbols)) {
  if ($symbols[$i] != $s) {
  $res = _append($res, $symbols[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function dpll_algorithm(&$clauses, $symbols, $model) {
  global $clause1, $clause2, $formula, $formula_str, $result;
  $all_true = true;
  $i = 0;
  while ($i < count($clauses)) {
  $ev = evaluate_clause($clauses[$i], $model);
  $clauses[$i] = $ev['clause'];
  if ($ev['value'] == 0) {
  return ['sat' => false, 'model' => []];
} else {
  if ($ev['value'] == 0 - 1) {
  $all_true = false;
};
}
  $i = $i + 1;
};
  if ($all_true) {
  return ['sat' => true, 'model' => $model];
}
  $p = $symbols[0];
  $rest = remove_symbol($symbols, $p);
  $tmp1 = $model;
  $tmp2 = $model;
  $tmp1[$p] = 1;
  $tmp2[$p] = 0;
  $res1 = dpll_algorithm($clauses, $rest, $tmp1);
  if ($res1['sat']) {
  return $res1;
}
  return dpll_algorithm($clauses, $rest, $tmp2);
};
  function str_clause($c) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $symbols, $model, $result;
  $line = '{';
  $first = true;
  $i = 0;
  while ($i < _len($c['names'])) {
  $lit = $c['names'][$i];
  if ($first) {
  $first = false;
} else {
  $line = $line . ' , ';
}
  $line = $line . $lit;
  $i = $i + 1;
};
  $line = $line . '}';
  return $line;
};
  function str_formula($f) {
  global $clause1, $clause2, $formula, $formula_str, $clauses, $symbols, $model, $result;
  $line = '{';
  $i = 0;
  while ($i < _len($f['clauses'])) {
  $line = $line . str_clause($f['clauses'][$i]);
  if ($i < _len($f['clauses']) - 1) {
  $line = $line . ' , ';
}
  $i = $i + 1;
};
  $line = $line . '}';
  return $line;
};
  $clause1 = new_clause(['A4', 'A3', 'A5\'', 'A1', 'A3\'']);
  $clause2 = new_clause(['A4']);
  $formula = new_formula([$clause1, $clause2]);
  $formula_str = str_formula($formula);
  $clauses = [$clause1, $clause2];
  $symbols = ['A4', 'A3', 'A5', 'A1'];
  $model = [];
  $result = dpll_algorithm($clauses, $symbols, $model);
  if ($result['sat']) {
  echo rtrim('The formula ' . $formula_str . ' is satisfiable.'), PHP_EOL;
} else {
  echo rtrim('The formula ' . $formula_str . ' is not satisfiable.'), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
