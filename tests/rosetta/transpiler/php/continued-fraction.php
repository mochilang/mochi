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
  function newTerm($a, $b) {
  return ['a' => $a, 'b' => $b];
};
  function cfSqrt2($nTerms) {
  $f = [];
  $n = 0;
  while ($n < $nTerms) {
  $f = array_merge($f, [newTerm(2, 1)]);
  $n = $n + 1;
};
  if ($nTerms > 0) {
  $f[0]['a'] = 1;
}
  return $f;
};
  function cfNap($nTerms) {
  $f = [];
  $n = 0;
  while ($n < $nTerms) {
  $f = array_merge($f, [newTerm($n, $n - 1)]);
  $n = $n + 1;
};
  if ($nTerms > 0) {
  $f[0]['a'] = 2;
}
  if ($nTerms > 1) {
  $f[1]['b'] = 1;
}
  return $f;
};
  function cfPi($nTerms) {
  $f = [];
  $n = 0;
  while ($n < $nTerms) {
  $g = 2 * $n - 1;
  $f = array_merge($f, [newTerm(6, $g * $g)]);
  $n = $n + 1;
};
  if ($nTerms > 0) {
  $f[0]['a'] = 3;
}
  return $f;
};
  function real($f) {
  $r = 0.0;
  $i = count($f) - 1;
  while ($i > 0) {
  $r = (floatval($f[$i]['b'])) / ((floatval($f[$i]['a'])) + $r);
  $i = $i - 1;
};
  if (count($f) > 0) {
  $r = $r + (floatval($f[0]['a']));
}
  return $r;
};
  function main() {
  echo rtrim('sqrt2: ' . _str(real(cfSqrt2(20)))), PHP_EOL;
  echo rtrim('nap:   ' . _str(real(cfNap(20)))), PHP_EOL;
  echo rtrim('pi:    ' . _str(real(cfPi(20)))), PHP_EOL;
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
