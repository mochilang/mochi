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
function populate_current_row($triangle, $current_row_idx) {
  $row = [];
  $i = 0;
  while ($i <= $current_row_idx) {
  if ($i == 0 || $i == $current_row_idx) {
  $row = _append($row, 1);
} else {
  $left = $triangle[$current_row_idx - 1][$i - 1];
  $right = $triangle[$current_row_idx - 1][$i];
  $row = _append($row, $left + $right);
}
  $i = $i + 1;
};
  return $row;
}
function generate_pascal_triangle($num_rows) {
  if ($num_rows <= 0) {
  return [];
}
  $triangle = [];
  $row_idx = 0;
  while ($row_idx < $num_rows) {
  $row = populate_current_row($triangle, $row_idx);
  $triangle = _append($triangle, $row);
  $row_idx = $row_idx + 1;
};
  return $triangle;
}
function row_to_string($row, $total_rows, $row_idx) {
  $line = '';
  $spaces = $total_rows - $row_idx - 1;
  $s = 0;
  while ($s < $spaces) {
  $line = $line . ' ';
  $s = $s + 1;
};
  $c = 0;
  while ($c <= $row_idx) {
  $line = $line . _str($row[$c]);
  if ($c != $row_idx) {
  $line = $line . ' ';
}
  $c = $c + 1;
};
  return $line;
}
function print_pascal_triangle($num_rows) {
  $triangle = generate_pascal_triangle($num_rows);
  $r = 0;
  while ($r < $num_rows) {
  $line = row_to_string($triangle[$r], $num_rows, $r);
  echo rtrim($line), PHP_EOL;
  $r = $r + 1;
};
}
function main() {
  print_pascal_triangle(5);
  echo rtrim(_str(generate_pascal_triangle(5))), PHP_EOL;
}
main();
