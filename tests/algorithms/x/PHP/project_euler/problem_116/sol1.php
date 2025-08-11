<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function solution($length) {
  $ways = [];
  $i = 0;
  while ($i <= $length) {
  $row = [];
  $row = _append($row, 0);
  $row = _append($row, 0);
  $row = _append($row, 0);
  $ways = _append($ways, $row);
  $i = _iadd($i, 1);
};
  $row_length = 0;
  while ($row_length <= $length) {
  $tile_length = 2;
  while ($tile_length <= 4) {
  $tile_start = 0;
  while ($tile_start <= _isub($row_length, $tile_length)) {
  $remaining = _isub(_isub($row_length, $tile_start), $tile_length);
  $ways[$row_length][_isub($tile_length, 2)] = _iadd(_iadd($ways[$row_length][_isub($tile_length, 2)], $ways[$remaining][_isub($tile_length, 2)]), 1);
  $tile_start = _iadd($tile_start, 1);
};
  $tile_length = _iadd($tile_length, 1);
};
  $row_length = _iadd($row_length, 1);
};
  $total = 0;
  $j = 0;
  while ($j < 3) {
  $total = _iadd($total, $ways[$length][$j]);
  $j = _iadd($j, 1);
};
  return $total;
}
echo rtrim(json_encode(solution(5), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(50), 1344)), PHP_EOL;
