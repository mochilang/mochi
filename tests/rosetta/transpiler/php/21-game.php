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
  $total = 0;
  $computer = fmod(_now(), 2) == 0;
  echo rtrim('Enter q to quit at any time
'), PHP_EOL;
  if ($computer) {
  echo rtrim('The computer will choose first'), PHP_EOL;
} else {
  echo rtrim('You will choose first'), PHP_EOL;
}
  echo rtrim('

Running total is now 0

'), PHP_EOL;
  $round = 1;
  $done = false;
  while (!$done) {
  echo rtrim('ROUND ' . _str($round) . ':

'), PHP_EOL;
  $i = 0;
  while ($i < 2 && (!$done)) {
  if ($computer) {
  $choice = 0;
  if ($total < 18) {
  $choice = fmod(_now(), 3) + 1;
} else {
  $choice = 21 - $total;
};
  $total = $total + $choice;
  echo rtrim('The computer chooses ' . _str($choice)), PHP_EOL;
  echo rtrim('Running total is now ' . _str($total)), PHP_EOL;
  if ($total == 21) {
  echo rtrim('
So, commiserations, the computer has won!'), PHP_EOL;
  $done = true;
};
} else {
  while (true) {
  echo rtrim('Your choice 1 to 3 : '), PHP_EOL;
  $line = trim(fgets(STDIN));
  if ($line == 'q' || $line == 'Q') {
  echo rtrim('OK, quitting the game'), PHP_EOL;
  $done = true;
  break;
}
  $num = parseIntStr($line, 10);
  if ($num < 1 || $num > 3) {
  if ($total + $num > 21) {
  echo rtrim('Too big, try again'), PHP_EOL;
} else {
  echo rtrim('Out of range, try again'), PHP_EOL;
};
  continue;
}
  if ($total + $num > 21) {
  echo rtrim('Too big, try again'), PHP_EOL;
  continue;
}
  $total = $total + $num;
  echo rtrim('Running total is now ' . _str($total)), PHP_EOL;
  break;
};
  if ($total == 21) {
  echo rtrim('
So, congratulations, you\'ve won!'), PHP_EOL;
  $done = true;
};
}
  echo rtrim('
'), PHP_EOL;
  $computer = !$computer;
  $i = $i + 1;
};
  $round = $round + 1;
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
