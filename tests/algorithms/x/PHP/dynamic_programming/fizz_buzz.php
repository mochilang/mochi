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
function fizz_buzz($number, $iterations) {
  if ($number < 1) {
  $panic('starting number must be an integer and be more than 0');
}
  if ($iterations < 1) {
  $panic('Iterations must be done more than 0 times to play FizzBuzz');
}
  $out = '';
  $n = $number;
  while ($n <= $iterations) {
  if ($n % 3 == 0) {
  $out = $out . 'Fizz';
}
  if ($n % 5 == 0) {
  $out = $out . 'Buzz';
}
  if ($n % 3 != 0 && $n % 5 != 0) {
  $out = $out . _str($n);
}
  $out = $out . ' ';
  $n = $n + 1;
};
  return $out;
}
echo rtrim(fizz_buzz(1, 7)), PHP_EOL;
echo rtrim(fizz_buzz(1, 15)), PHP_EOL;
