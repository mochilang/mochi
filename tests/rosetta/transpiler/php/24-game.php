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
$__start_mem = memory_get_usage();
$__start = _now();
  function randDigit() {
  return (fmod(_now(), 9)) + 1;
};
  function main() {
  $digits = [];
  for ($i = 0; $i < 4; $i++) {
  $digits = array_merge($digits, [randDigit()]);
};
  $numstr = '';
  for ($i = 0; $i < 4; $i++) {
  $numstr = $numstr . _str($digits[$i]);
};
  echo rtrim('Your numbers: ' . $numstr . '
'), PHP_EOL;
  echo rtrim('Enter RPN: '), PHP_EOL;
  $expr = trim(fgets(STDIN));
  if (strlen($expr) != 7) {
  echo rtrim('invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)'), PHP_EOL;
  return;
}
  $stack = [];
  $i = 0;
  $valid = true;
  while ($i < strlen($expr)) {
  $ch = substr($expr, $i, $i + 1 - $i);
  if ($ch >= '0' && $ch <= '9') {
  if (count($digits) == 0) {
  echo rtrim('too many numbers.'), PHP_EOL;
  return;
};
  $j = 0;
  while ($digits[$j] != intval($ch) - intval('0')) {
  $j = $j + 1;
  if ($j == count($digits)) {
  echo rtrim('wrong numbers.'), PHP_EOL;
  return;
}
};
  $digits = array_merge(array_slice($digits, 0, $j - 0), array_slice($digits, $j + 1));
  $stack = array_merge($stack, [floatval(intval($ch) - intval('0'))]);
} else {
  if (count($stack) < 2) {
  echo rtrim('invalid expression syntax.'), PHP_EOL;
  $valid = false;
  break;
};
  $b = $stack[count($stack) - 1];
  $a = $stack[count($stack) - 2];
  if ($ch == '+') {
  $stack[count($stack) - 2] = $a + $b;
} else {
  if ($ch == '-') {
  $stack[count($stack) - 2] = $a - $b;
} else {
  if ($ch == '*') {
  $stack[count($stack) - 2] = $a * $b;
} else {
  if ($ch == '/') {
  $stack[count($stack) - 2] = $a / $b;
} else {
  echo rtrim($ch . ' invalid.'), PHP_EOL;
  $valid = false;
  break;
};
};
};
};
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
}
  $i = $i + 1;
};
  if ($valid) {
  if ($abs($stack[0] - 24.0) > 0.000001) {
  echo rtrim('incorrect. ' . _str($stack[0]) . ' != 24'), PHP_EOL;
} else {
  echo rtrim('correct.'), PHP_EOL;
};
}
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
