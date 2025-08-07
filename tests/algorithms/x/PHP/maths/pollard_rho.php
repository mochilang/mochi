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
  function gcd($a, $b) {
  $x = ($a < 0 ? -$a : $a);
  $y = ($b < 0 ? -$b : $b);
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
};
  function rand_fn($value, $step, $modulus) {
  return ($value * $value + $step) % $modulus;
};
  function pollard_rho($num, $seed, $step, $attempts) {
  if ($num < 2) {
  $panic('The input value cannot be less than 2');
}
  if ($num > 2 && $num % 2 == 0) {
  return ['factor' => 2, 'ok' => true];
}
  $s = $seed;
  $st = $step;
  $i = 0;
  while ($i < $attempts) {
  $tortoise = $s;
  $hare = $s;
  while (true) {
  $tortoise = rand_fn($tortoise, $st, $num);
  $hare = rand_fn($hare, $st, $num);
  $hare = rand_fn($hare, $st, $num);
  $divisor = gcd($hare - $tortoise, $num);
  if ($divisor == 1) {
  continue;
} else {
  if ($divisor == $num) {
  break;
} else {
  return ['factor' => $divisor, 'ok' => true];
};
}
};
  $s = $hare;
  $st = $st + 1;
  $i = $i + 1;
};
  return ['factor' => 0, 'ok' => false];
};
  function test_pollard_rho() {
  $r1 = pollard_rho(8051, 2, 1, 5);
  if (!$r1['ok'] || ($r1['factor'] != 83 && $r1['factor'] != 97)) {
  $panic('test1 failed');
}
  $r2 = pollard_rho(10403, 2, 1, 5);
  if (!$r2['ok'] || ($r2['factor'] != 101 && $r2['factor'] != 103)) {
  $panic('test2 failed');
}
  $r3 = pollard_rho(100, 2, 1, 3);
  if (!$r3['ok'] || $r3['factor'] != 2) {
  $panic('test3 failed');
}
  $r4 = pollard_rho(17, 2, 1, 3);
  if ($r4['ok']) {
  $panic('test4 failed');
}
  $r5 = pollard_rho(17 * 17 * 17, 2, 1, 3);
  if (!$r5['ok'] || $r5['factor'] != 17) {
  $panic('test5 failed');
}
  $r6 = pollard_rho(17 * 17 * 17, 2, 1, 1);
  if ($r6['ok']) {
  $panic('test6 failed');
}
  $r7 = pollard_rho(3 * 5 * 7, 2, 1, 3);
  if (!$r7['ok'] || $r7['factor'] != 21) {
  $panic('test7 failed');
}
};
  function main() {
  test_pollard_rho();
  $a = pollard_rho(100, 2, 1, 3);
  if ($a['ok']) {
  echo rtrim(_str($a['factor'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
  $b = pollard_rho(17, 2, 1, 3);
  if ($b['ok']) {
  echo rtrim(_str($b['factor'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
