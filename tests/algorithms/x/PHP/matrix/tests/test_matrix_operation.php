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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function check_matrix($mat) {
  if (count($mat) < 2 || count($mat[0]) < 2) {
  _panic('Expected a matrix with at least 2x2 dimensions');
}
}
function add($a, $b) {
  check_matrix($a);
  check_matrix($b);
  if (count($a) != count($b) || count($a[0]) != count($b[0])) {
  _panic('Matrices must have the same dimensions');
}
  $rows = count($a);
  $cols = count($a[0]);
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, $a[$i][$j] + $b[$i][$j]);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function subtract($a, $b) {
  check_matrix($a);
  check_matrix($b);
  if (count($a) != count($b) || count($a[0]) != count($b[0])) {
  _panic('Matrices must have the same dimensions');
}
  $rows = count($a);
  $cols = count($a[0]);
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, $a[$i][$j] - $b[$i][$j]);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function scalar_multiply($a, $s) {
  check_matrix($a);
  $rows = count($a);
  $cols = count($a[0]);
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, $a[$i][$j] * $s);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function multiply($a, $b) {
  check_matrix($a);
  check_matrix($b);
  if (count($a[0]) != count($b)) {
  _panic('Invalid dimensions for matrix multiplication');
}
  $rows = count($a);
  $cols = count($b[0]);
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $sum = 0.0;
  $k = 0;
  while ($k < count($b)) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function identity($n) {
  $result = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function transpose($a) {
  check_matrix($a);
  $rows = count($a);
  $cols = count($a[0]);
  $result = [];
  $j = 0;
  while ($j < $cols) {
  $row = [];
  $i = 0;
  while ($i < $rows) {
  $row = _append($row, $a[$i][$j]);
  $i = $i + 1;
};
  $result = _append($result, $row);
  $j = $j + 1;
};
  return $result;
}
function main() {
  $mat_a = [[12.0, 10.0], [3.0, 9.0]];
  $mat_b = [[3.0, 4.0], [7.0, 4.0]];
  $mat_c = [[3.0, 0.0, 2.0], [2.0, 0.0, -2.0], [0.0, 1.0, 1.0]];
  echo rtrim(_str(add($mat_a, $mat_b))), PHP_EOL;
  echo rtrim(_str(subtract($mat_a, $mat_b))), PHP_EOL;
  echo rtrim(_str(multiply($mat_a, $mat_b))), PHP_EOL;
  echo rtrim(_str(scalar_multiply($mat_a, 3.5))), PHP_EOL;
  echo rtrim(_str(identity(5))), PHP_EOL;
  echo rtrim(_str(transpose($mat_c))), PHP_EOL;
}
main();
