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
  function mochi_xor($a, $b) {
  return ($a && (!$b)) || ((!$a) && $b);
};
  function ha($a, $b) {
  return ['s' => mochi_xor($a, $b), 'c' => $a && $b];
};
  function fa($a, $b, $c0) {
  $r1 = ha($a, $c0);
  $r2 = ha($r1['s'], $b);
  return ['s' => $r2['s'], 'c' => $r1['c'] || $r2['c']];
};
  function add4($a3, $a2, $a1, $a0, $b3, $b2, $b1, $b0) {
  $r0 = fa($a0, $b0, false);
  $r1 = fa($a1, $b1, $r0['c']);
  $r2 = fa($a2, $b2, $r1['c']);
  $r3 = fa($a3, $b3, $r2['c']);
  return ['v' => $r3['c'], 's3' => $r3['s'], 's2' => $r2['s'], 's1' => $r1['s'], 's0' => $r0['s']];
};
  function b2i($b) {
  if ($b) {
  return 1;
}
  return 0;
};
  function main() {
  $r = add4(true, false, true, false, true, false, false, true);
  echo rtrim(_str(b2i($r['v'])) . ' ' . _str(b2i($r['s3'])) . ' ' . _str(b2i($r['s2'])) . ' ' . _str(b2i($r['s1'])) . ' ' . _str(b2i($r['s0']))), PHP_EOL;
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
