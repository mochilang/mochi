<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
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
function make_matrix(&$rows, &$cols, $value) {
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
  return ['cols' => &$cols, 'data' => &$arr, 'rows' => &$rows];
}
function matrix_from_lists($vals) {
  $r = count($vals);
  $c = ($r == 0 ? 0 : count($vals[0]));
  return ['cols' => &$c, 'data' => &$vals, 'rows' => &$r];
}
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
}
function matrix_add($a, $b) {
  if ($a['rows'] != $b['rows'] || $a['cols'] != $b['cols']) {
  return ['cols' => 0, 'data' => [], 'rows' => 0];
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
  return ['cols' => $a['cols'], 'data' => &$res, 'rows' => $a['rows']];
}
function matrix_sub($a, $b) {
  if ($a['rows'] != $b['rows'] || $a['cols'] != $b['cols']) {
  return ['cols' => 0, 'data' => [], 'rows' => 0];
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
  return ['cols' => $a['cols'], 'data' => &$res, 'rows' => $a['rows']];
}
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
  return ['cols' => $m['cols'], 'data' => &$res, 'rows' => $m['rows']];
}
function matrix_mul($a, $b) {
  if ($a['cols'] != $b['rows']) {
  return ['cols' => 0, 'data' => [], 'rows' => 0];
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
  return ['cols' => $b['cols'], 'data' => &$res, 'rows' => $a['rows']];
}
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
  return ['cols' => $m['rows'], 'data' => &$res, 'rows' => $m['cols']];
}
function sherman_morrison($ainv, $u, $v) {
  $vt = matrix_transpose($v);
  $vu = matrix_mul(matrix_mul($vt, $ainv), $u);
  $factor = $vu['data'][0][0] + 1.0;
  if ($factor == 0.0) {
  return ['cols' => 0, 'data' => [], 'rows' => 0];
}
  $term1 = matrix_mul($ainv, $u);
  $term2 = matrix_mul($vt, $ainv);
  $numerator = matrix_mul($term1, $term2);
  $scaled = matrix_mul_scalar($numerator, 1.0 / $factor);
  return matrix_sub($ainv, $scaled);
}
function main() {
  $ainv = matrix_from_lists([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
  $u = matrix_from_lists([[1.0], [2.0], [-3.0]]);
  $v = matrix_from_lists([[4.0], [-2.0], [5.0]]);
  $result = sherman_morrison($ainv, $u, $v);
  echo rtrim(matrix_to_string($result)), PHP_EOL;
}
main();
