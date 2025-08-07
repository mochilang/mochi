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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pivot(&$t, $row, $col) {
  global $tableau, $finalTab, $res, $key;
  $pivotRow = [];
  $pivotVal = $t[$row][$col];
  for ($j = 0; $j < count($t[$row]); $j++) {
  $pivotRow = _append($pivotRow, $t[$row][$j] / $pivotVal);
};
  $t[$row] = $pivotRow;
  for ($i = 0; $i < count($t); $i++) {
  if ($i != $row) {
  $factor = $t[$i][$col];
  $newRow = [];
  for ($j = 0; $j < count($t[$i]); $j++) {
  $value = $t[$i][$j] - $factor * $pivotRow[$j];
  $newRow = _append($newRow, $value);
};
  $t[$i] = $newRow;
}
};
  return $t;
};
  function findPivot($t) {
  global $tableau, $finalTab, $res, $key;
  $col = 0;
  $minVal = 0.0;
  for ($j = 0; $j < count($t[0]) - 1; $j++) {
  $v = $t[0][$j];
  if ($v < $minVal) {
  $minVal = $v;
  $col = $j;
}
};
  if ($minVal >= 0.0) {
  return [-1, -1];
}
  $row = -1;
  $minRatio = 0.0;
  $first = true;
  for ($i = 1; $i < count($t); $i++) {
  $coeff = $t[$i][$col];
  if ($coeff > 0.0) {
  $rhs = $t[$i][count($t[$i]) - 1];
  $ratio = $rhs / $coeff;
  if ($first || $ratio < $minRatio) {
  $minRatio = $ratio;
  $row = $i;
  $first = false;
};
}
};
  return [$row, $col];
};
  function interpret($t, $nVars) {
  global $tableau, $finalTab, $res, $key;
  $lastCol = count($t[0]) - 1;
  $p = $t[0][$lastCol];
  if ($p < 0.0) {
  $p = -$p;
}
  $result = [];
  $result['P'] = $p;
  for ($i = 0; $i < $nVars; $i++) {
  $nzRow = -1;
  $nzCount = 0;
  for ($r = 0; $r < count($t); $r++) {
  $val = $t[$r][$i];
  if ($val != 0.0) {
  $nzCount = $nzCount + 1;
  $nzRow = $r;
}
};
  if ($nzCount == 1 && $t[$nzRow][$i] == 1.0) {
  $result['x' . _str($i + 1)] = $t[$nzRow][$lastCol];
}
};
  return $result;
};
  function simplex($tab) {
  global $tableau, $finalTab, $res, $key;
  $t = $tab;
  while (true) {
  $p = findPivot($t);
  $row = $p[0];
  $col = $p[1];
  if ($row < 0) {
  break;
}
  $t = pivot($t, $row, $col);
};
  return $t;
};
  $tableau = [[-1.0, -1.0, 0.0, 0.0, 0.0], [1.0, 3.0, 1.0, 0.0, 4.0], [3.0, 1.0, 0.0, 1.0, 4.0]];
  $finalTab = simplex($tableau);
  $res = interpret($finalTab, 2);
  echo rtrim('P: ' . _str($res['P'])), PHP_EOL;
  for ($i = 0; $i < 2; $i++) {
  $key = 'x' . _str($i + 1);
  if (array_key_exists($key, $res)) {
  echo rtrim($key . ': ' . _str($res[$key])), PHP_EOL;
}
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
