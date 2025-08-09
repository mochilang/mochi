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
function power_of_two($exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
}
function solution($power) {
  $num = power_of_two($power);
  $string_num = _str($num);
  $sum = 0;
  $i = 0;
  while ($i < strlen($string_num)) {
  $sum = $sum + ((ctype_digit($string_num[$i]) ? intval($string_num[$i]) : ord($string_num[$i])));
  $i = $i + 1;
};
  return $sum;
}
echo rtrim(_str(solution(1000))), PHP_EOL;
echo rtrim(_str(solution(50))), PHP_EOL;
echo rtrim(_str(solution(20))), PHP_EOL;
echo rtrim(_str(solution(15))), PHP_EOL;
