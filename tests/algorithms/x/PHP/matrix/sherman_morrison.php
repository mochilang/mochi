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
  function make_matrix($rows, $cols, $value) {
  $arr = [];
  $r = 0;
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $cols) {
  $row = _append($row, $value);
  $c = $c + 1;
};
  $arr = _append($arr, $row);
  $r = $r + 1;
};
  return ['data' => $arr, 'rows' => $rows, 'cols' => $cols];
};
  function matrix_from_lists($vals) {
  $r = count($vals);
  $c = ($r == 0 ? 0 : count($vals[0]));
  return ['data' => $vals, 'rows' => $r, 'cols' => $c];
};
  function matrix_to_string($m) {
  $s = '';
  $i = 0;
  while ($i < $m['rows']) {
  $s = $s . '[';
  $j = 0;
  while ($j < $m['cols']) {
  $s = $s . _str($m['data'][$i][$j]);
  if ($j < $m['cols'] - 1) {
  $s = $s . ', ';
}
  $j = $j + 1;
};
  $s = $s . ']';
  if ($i < $m['rows'] - 1) {
  $s = $s . '
';
}
  $i = $i + 1;
};
  return $s;
};
  function matrix_add($a, $b) {
  if ($a['rows'] != $b['rows'] || $a['cols'] != $b['cols']) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $res = [];
  $i = 0;
  while ($i < $a['rows']) {
  $row = [];
  $j = 0;
  while ($j < $a['cols']) {
  $row = _append($row, $a['data'][$i][$j] + $b['data'][$i][$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return ['data' => $res, 'rows' => $a['rows'], 'cols' => $a['cols']];
};
  function matrix_sub($a, $b) {
  if ($a['rows'] != $b['rows'] || $a['cols'] != $b['cols']) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $res = [];
  $i = 0;
  while ($i < $a['rows']) {
  $row = [];
  $j = 0;
  while ($j < $a['cols']) {
  $row = _append($row, $a['data'][$i][$j] - $b['data'][$i][$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return ['data' => $res, 'rows' => $a['rows'], 'cols' => $a['cols']];
};
  function matrix_mul_scalar($m, $k) {
  $res = [];
  $i = 0;
  while ($i < $m['rows']) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  $row = _append($row, $m['data'][$i][$j] * $k);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return ['data' => $res, 'rows' => $m['rows'], 'cols' => $m['cols']];
};
  function matrix_mul($a, $b) {
  if ($a['cols'] != $b['rows']) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $res = [];
  $i = 0;
  while ($i < $a['rows']) {
  $row = [];
  $j = 0;
  while ($j < $b['cols']) {
  $sum = 0.0;
  $k = 0;
  while ($k < $a['cols']) {
  $sum = $sum + $a['data'][$i][$k] * $b['data'][$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return ['data' => $res, 'rows' => $a['rows'], 'cols' => $b['cols']];
};
  function matrix_transpose($m) {
  $res = [];
  $c = 0;
  while ($c < $m['cols']) {
  $row = [];
  $r = 0;
  while ($r < $m['rows']) {
  $row = _append($row, $m['data'][$r][$c]);
  $r = $r + 1;
};
  $res = _append($res, $row);
  $c = $c + 1;
};
  return ['data' => $res, 'rows' => $m['cols'], 'cols' => $m['rows']];
};
  function sherman_morrison($ainv, $u, $v) {
  $vt = matrix_transpose($v);
  $vu = matrix_mul(matrix_mul($vt, $ainv), $u);
  $factor = $vu['data'][0][0] + 1.0;
  if ($factor == 0.0) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $term1 = matrix_mul($ainv, $u);
  $term2 = matrix_mul($vt, $ainv);
  $numerator = matrix_mul($term1, $term2);
  $scaled = matrix_mul_scalar($numerator, 1.0 / $factor);
  return matrix_sub($ainv, $scaled);
};
  function main() {
  $ainv = matrix_from_lists([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
  $u = matrix_from_lists([[1.0], [2.0], [-3.0]]);
  $v = matrix_from_lists([[4.0], [-2.0], [5.0]]);
  $result = sherman_morrison($ainv, $u, $v);
  echo rtrim(matrix_to_string($result)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
