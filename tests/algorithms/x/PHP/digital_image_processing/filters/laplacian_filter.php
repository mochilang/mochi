<?php
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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function make_matrix($rows, $cols, $value) {
  global $image, $r, $row_str, $c;
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, $value);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function my_laplacian($src, $ksize) {
  global $image, $result, $r, $row_str, $c;
  $kernel = [];
  if ($ksize == 1) {
  $kernel = [[0, -1, 0], [-1, 4, -1], [0, -1, 0]];
} else {
  if ($ksize == 3) {
  $kernel = [[0, 1, 0], [1, -4, 1], [0, 1, 0]];
} else {
  if ($ksize == 5) {
  $kernel = [[0, 0, -1, 0, 0], [0, -1, -2, -1, 0], [-1, -2, 16, -2, -1], [0, -1, -2, -1, 0], [0, 0, -1, 0, 0]];
} else {
  if ($ksize == 7) {
  $kernel = [[0, 0, 0, -1, 0, 0, 0], [0, 0, -2, -3, -2, 0, 0], [0, -2, -7, -10, -7, -2, 0], [-1, -3, -10, 68, -10, -3, -1], [0, -2, -7, -10, -7, -2, 0], [0, 0, -2, -3, -2, 0, 0], [0, 0, 0, -1, 0, 0, 0]];
} else {
  $panic('ksize must be in (1, 3, 5, 7)');
};
};
};
}
  $rows = count($src);
  $cols = count($src[0]);
  $k = count($kernel);
  $pad = _intdiv($k, 2);
  $output = make_matrix($rows, $cols, 0);
  $i = 0;
  while ($i < $rows) {
  $j = 0;
  while ($j < $cols) {
  $sum = 0;
  $ki = 0;
  while ($ki < $k) {
  $kj = 0;
  while ($kj < $k) {
  $ii = $i + $ki - $pad;
  $jj = $j + $kj - $pad;
  $val = 0;
  if ($ii >= 0 && $ii < $rows && $jj >= 0 && $jj < $cols) {
  $val = $src[$ii][$jj];
}
  $sum = $sum + $val * $kernel[$ki][$kj];
  $kj = $kj + 1;
};
  $ki = $ki + 1;
};
  $output[$i][$j] = $sum;
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $output;
}
$image = [[0, 0, 0, 0, 0], [0, 10, 10, 10, 0], [0, 10, 10, 10, 0], [0, 10, 10, 10, 0], [0, 0, 0, 0, 0]];
$result = my_laplacian($image, 3);
$r = 0;
while ($r < count($result)) {
  $row_str = '[';
  $c = 0;
  while ($c < count($result[$r])) {
  $row_str = $row_str . _str($result[$r][$c]);
  if ($c + 1 < count($result[$r])) {
  $row_str = $row_str . ', ';
}
  $c = $c + 1;
};
  $row_str = $row_str . ']';
  echo rtrim($row_str), PHP_EOL;
  $r = $r + 1;
}
