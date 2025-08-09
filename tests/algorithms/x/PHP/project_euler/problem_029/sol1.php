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
function int_pow($base, $exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = _imul($result, $base);
  $i = _iadd($i, 1);
};
  return $result;
}
function solution($n) {
  $powers = [];
  $limit = _iadd($n, 1);
  for ($a = 2; $a < $limit; $a++) {
  for ($b = 2; $b < $limit; $b++) {
  $p = int_pow($a, $b);
  if (!(in_array($p, $powers))) {
  $powers = _append($powers, $p);
}
};
};
  return count($powers);
}
function main() {
  $n = intval(trim(fgets(STDIN)));
  echo rtrim('Number of terms ') . " " . rtrim(json_encode(solution($n), 1344)), PHP_EOL;
}
main();
