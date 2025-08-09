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
function solution($pence) {
  $coins = [1, 2, 5, 10, 20, 50, 100, 200];
  $ways = [];
  $i = 0;
  while ($i <= $pence) {
  $ways = _append($ways, 0);
  $i = _iadd($i, 1);
};
  $ways[0] = 1;
  $idx = 0;
  while ($idx < count($coins)) {
  $coin = $coins[$idx];
  $j = $coin;
  while ($j <= $pence) {
  $ways[$j] = _iadd($ways[$j], $ways[_isub($j, $coin)]);
  $j = _iadd($j, 1);
};
  $idx = _iadd($idx, 1);
};
  return $ways[$pence];
}
echo rtrim(json_encode(solution(500), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(200), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(50), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(10), 1344)), PHP_EOL;
