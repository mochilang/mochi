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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function divisors($n) {
  $divs = [1];
  $divs2 = [];
  $i = 2;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  $j = intval((_intdiv($n, $i)));
  $divs = array_merge($divs, [$i]);
  if ($i != $j) {
  $divs2 = array_merge($divs2, [$j]);
};
}
  $i = $i + 1;
};
  $j = count($divs2) - 1;
  while ($j >= 0) {
  $divs = array_merge($divs, [$divs2[$j]]);
  $j = $j - 1;
};
  return $divs;
};
  function sum($xs) {
  $tot = 0;
  foreach ($xs as $v) {
  $tot = $tot + $v;
};
  return $tot;
};
  function sumStr($xs) {
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]) . ' + ';
  $i = $i + 1;
};
  return substr($s, 0, strlen($s) - 3 - 0);
};
  function pad2($n) {
  $s = _str($n);
  if (strlen($s) < 2) {
  return ' ' . $s;
}
  return $s;
};
  function pad5($n) {
  $s = _str($n);
  while (strlen($s) < 5) {
  $s = ' ' . $s;
};
  return $s;
};
  function abundantOdd($searchFrom, $countFrom, $countTo, $printOne) {
  $count = $countFrom;
  $n = $searchFrom;
  while ($count < $countTo) {
  $divs = divisors($n);
  $tot = array_sum($divs);
  if ($tot > $n) {
  $count = $count + 1;
  if ($printOne && $count < $countTo) {
  $n = $n + 2;
  continue;
};
  $s = sumStr($divs);
  if (!$printOne) {
  echo rtrim(pad2($count) . '. ' . pad5($n) . ' < ' . $s . ' = ' . _str($tot)), PHP_EOL;
} else {
  echo rtrim(_str($n) . ' < ' . $s . ' = ' . _str($tot)), PHP_EOL;
};
}
  $n = $n + 2;
};
  return $n;
};
  function main() {
  $max = 25;
  echo rtrim('The first ' . _str($max) . ' abundant odd numbers are:'), PHP_EOL;
  $n = abundantOdd(1, 0, $max, false);
  echo rtrim('\nThe one thousandth abundant odd number is:'), PHP_EOL;
  abundantOdd($n, $max, 1000, true);
  echo rtrim('\nThe first abundant odd number above one billion is:'), PHP_EOL;
  abundantOdd(1000000001, 0, 1, true);
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
