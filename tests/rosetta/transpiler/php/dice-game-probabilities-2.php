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
  function roll($nDice, $nSides) {
  $sum = 0;
  $i = 0;
  while ($i < $nDice) {
  $sum = $sum + (fmod(_now(), $nSides)) + 1;
  $i = $i + 1;
};
  return $sum;
};
  function beats($n1, $s1, $n2, $s2, $trials) {
  $wins = 0;
  $i = 0;
  while ($i < $trials) {
  if (roll($n1, $s1) > roll($n2, $s2)) {
  $wins = $wins + 1;
}
  $i = $i + 1;
};
  return (floatval($wins)) / (floatval($trials));
};
  echo rtrim(_str(beats(9, 4, 6, 6, 1000))), PHP_EOL;
  echo rtrim(_str(beats(5, 10, 7, 6, 1000))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
