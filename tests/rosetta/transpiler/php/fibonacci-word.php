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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $math = ['sqrt' => function($x) {
  return sqrt($x);
}, 'pow' => function($x, $y) {
  return pow($x, $y);
}, 'sin' => function($x) {
  return sin($x);
}, 'log' => function($x) {
  return log($x);
}, 'pi' => M_PI, 'e' => M_E];
  function entropy($s) {
  global $math;
  $counts = [];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if (array_key_exists($ch, $counts)) {
  $counts[$ch] = $counts[$ch] + 1;
} else {
  $counts[$ch] = 1;
}
  $i = $i + 1;
};
  $hm = 0.0;
  foreach (array_keys($counts) as $k) {
  $c = floatval($counts[$k]);
  $hm = $hm + $c * ($math['log']($c) / $math['log'](2.0));
};
  $l = floatval(strlen($s));
  return ($math['log']($l) / $math['log'](2.0)) - $hm / $l;
};
  function fibonacciWord($n) {
  global $math;
  $a = '1';
  $b = '0';
  $i = 1;
  while ($i < $n) {
  $tmp = $b;
  $b = $b . $a;
  $a = $tmp;
  $i = $i + 1;
};
  return $a;
};
  function main() {
  global $math;
  echo rtrim(pad('N', 3) . pad('Length', 9) . '  Entropy      Word'), PHP_EOL;
  $n = 1;
  while ($n < 10) {
  $s = fibonacciWord($n);
  echo rtrim(pad(_str($n), 3) . pad(_str(strlen($s)), 9) . '  ' . fmt(entropy($s)) . '  ' . $s), PHP_EOL;
  $n = $n + 1;
};
  while ($n <= 37) {
  $s = fibonacciWord($n);
  echo rtrim(pad(_str($n), 3) . pad(_str(strlen($s)), 9) . '  ' . fmt(entropy($s))), PHP_EOL;
  $n = $n + 1;
};
};
  function pad($s, $w) {
  global $math;
  $t = $s;
  while (strlen($t) < $w) {
  $t = ' ' . $t;
};
  return $t;
};
  function fmt($x) {
  global $math;
  $y = floorf($x * 100000000.0 + 0.5) / 100000000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.00000000';
} else {
  $d = strlen($s) - $dot - 1;
  while ($d < 8) {
  $s = $s . '0';
  $d = $d + 1;
};
}
  return $s;
};
  function floorf($x) {
  global $math;
  $y = intval($x);
  return floatval($y);
};
  function indexOf($s, $ch) {
  global $math;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
