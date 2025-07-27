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
  function p($x, $e) {
  global $ops, $a, $b, $c, $d;
  $r = 1.0;
  $i = 0;
  while ($i < (intval($e))) {
  $r = $r * $x;
  $i = $i + 1;
};
  return $r;
};
  $ops = ['-x.p(e)', '-(x).p(e)', '(-x).p(e)', '-(x.p(e))'];
  foreach ([-5.0, 5.0] as $x) {
  foreach ([2.0, 3.0] as $e) {
  $a = -p($x, $e);
  $b = -(p($x, $e));
  $c = p(-$x, $e);
  $d = -(p($x, $e));
  echo rtrim('x = ' . (($x < 0 ? '' : ' ')) . _str((intval($x))) . ' e = ' . _str((intval($e))) . ' | ' . $ops[0] . ' = ' . padInt($a) . ' | ' . $ops[1] . ' = ' . padInt($b) . ' | ' . $ops[2] . ' = ' . padInt($c) . ' | ' . $ops[3] . ' = ' . padInt($d)), PHP_EOL;
};
}
  function padInt($f) {
  global $ops, $a, $b, $c, $d;
  $s = _str((intval($f)));
  if ($f >= 0) {
  return ' ' . $s;
}
  return $s;
};
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
