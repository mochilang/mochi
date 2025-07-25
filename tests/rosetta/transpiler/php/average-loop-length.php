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
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function floorf($x) {
  $y = intval($x);
  return floatval($y);
};
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function fmtF($x) {
  $y = floorf($x * 10000.0 + 0.5) / 10000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.0000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 4) {
  $s = substr($s, 0, $dot + 5 - 0);
} else {
  while ($decs < 4) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  return $s;
};
  function padInt($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function padFloat($x, $width) {
  $s = fmtF($x);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function avgLen($n) {
  $tests = 10000;
  $sum = 0;
  $seed = 1;
  $t = 0;
  while ($t < $tests) {
  $visited = [];
  $i = 0;
  while ($i < $n) {
  $visited = array_merge($visited, [false]);
  $i = $i + 1;
};
  $x = 0;
  while (!$visited[$x]) {
  $visited[$x] = true;
  $sum = $sum + 1;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  $x = $seed % $n;
};
  $t = $t + 1;
};
  return (floatval($sum)) / $tests;
};
  function ana($n) {
  $nn = floatval($n);
  $term = 1.0;
  $sum = 1.0;
  $i = $nn - 1.0;
  while ($i >= 1.0) {
  $term = $term * ($i / $nn);
  $sum = $sum + $term;
  $i = $i - 1.0;
};
  return $sum;
};
  function main() {
  $nmax = 20;
  echo rtrim(' N    average    analytical    (error)'), PHP_EOL;
  echo rtrim('===  =========  ============  ========='), PHP_EOL;
  $n = 1;
  while ($n <= $nmax) {
  $a = avgLen($n);
  $b = ana($n);
  $err = absf($a - $b) / $b * 100.0;
  $line = padInt($n, 3) . '  ' . padFloat($a, 9) . '  ' . padFloat($b, 12) . '  (' . padFloat($err, 6) . '%)';
  echo rtrim($line), PHP_EOL;
  $n = $n + 1;
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
