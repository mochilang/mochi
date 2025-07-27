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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_parseIntStr($str) {
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function main() {
  $n = 0;
  while ($n < 1 || $n > 5) {
  echo rtrim('How many integer variables do you want to create (max 5) : '), PHP_EOL;
  $line = trim(fgets(STDIN));
  if (strlen($line) > 0) {
  $n = parseIntStr($line, 10);
}
};
  $vars = [];
  echo rtrim('OK, enter the variable names and their values, below
'), PHP_EOL;
  $i = 1;
  while ($i <= $n) {
  echo rtrim('
  Variable ' . _str($i) . '
'), PHP_EOL;
  echo rtrim('    Name  : '), PHP_EOL;
  $name = trim(fgets(STDIN));
  if (array_key_exists($name, $vars)) {
  echo rtrim('  Sorry, you\'ve already created a variable of that name, try again'), PHP_EOL;
  continue;
}
  $value = 0;
  while (true) {
  echo rtrim('    Value : '), PHP_EOL;
  $valstr = trim(fgets(STDIN));
  if (strlen($valstr) == 0) {
  echo rtrim('  Not a valid integer, try again'), PHP_EOL;
  continue;
}
  $ok = true;
  $j = 0;
  $neg = false;
  if (substr($valstr, 0, 1 - 0) == '-') {
  $neg = true;
  $j = 1;
}
  while ($j < strlen($valstr)) {
  $ch = substr($valstr, $j, $j + 1 - $j);
  if ($ch < '0' || $ch > '9') {
  $ok = false;
  break;
}
  $j = $j + 1;
};
  if (!$ok) {
  echo rtrim('  Not a valid integer, try again'), PHP_EOL;
  continue;
}
  $value = parseIntStr($valstr, 10);
  break;
};
  $vars[$name] = $value;
  $i = $i + 1;
};
  echo rtrim('
Enter q to quit'), PHP_EOL;
  while (true) {
  echo rtrim('
Which variable do you want to inspect : '), PHP_EOL;
  $name = trim(fgets(STDIN));
  if (strtolower($name) == 'q') {
  return;
}
  if (array_key_exists($name, $vars)) {
  echo rtrim('It\'s value is ' . _str($vars[$name])), PHP_EOL;
} else {
  echo rtrim('Sorry there\'s no variable of that name, try again'), PHP_EOL;
}
};
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
