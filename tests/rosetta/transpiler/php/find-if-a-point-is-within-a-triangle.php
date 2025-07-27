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
  function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function area($x1, $y1, $x2, $y2, $x3, $y3) {
  return mochi_abs(($x1 * ($y2 - $y3) + $x2 * ($y3 - $y1) + $x3 * ($y1 - $y2)) / 2.0);
};
  function pointInTriangle($x1, $y1, $x2, $y2, $x3, $y3, $x, $y) {
  $A = area($x1, $y1, $x2, $y2, $x3, $y3);
  $A1 = area($x, $y, $x2, $y2, $x3, $y3);
  $A2 = area($x1, $y1, $x, $y, $x3, $y3);
  $A3 = area($x1, $y1, $x2, $y2, $x, $y);
  $diff = (floatval(mochi_abs(($A1 + $A2 + $A3) - $A)));
  if ($diff < 0.0001) {
  return true;
}
  return false;
};
  function main() {
  $tri1 = [[3.0 / 2, 12.0 / 5], [51.0 / 10, -31.0 / 10], [-19.0 / 5, 1.2]];
  echo rtrim('Triangle is ' . _str($tri1)), PHP_EOL;
  foreach ([[0.0, 0.0], [0.0, 1.0], [3.0, 1.0]] as $pt) {
  $ok = pointInTriangle($tri1[0][0], $tri1[0][1], $tri1[1][0], $tri1[1][1], $tri1[2][0], $tri1[2][1], $pt[0], $pt[1]);
  echo rtrim('Point ' . _str($pt) . ' is within triangle? ' . _str($ok)), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
  $tri2 = [[1.0 / 10, 1.0 / 9], [100.0 / 8, 100.0 / 3], [100.0 / 4, 100.0 / 9]];
  echo rtrim('Triangle is ' . _str($tri2)), PHP_EOL;
  $x = $tri2[0][0] + (3.0 / 7) * ($tri2[1][0] - $tri2[0][0]);
  $y = $tri2[0][1] + (3.0 / 7) * ($tri2[1][1] - $tri2[0][1]);
  $pt = [$x, $y];
  $ok = pointInTriangle($tri2[0][0], $tri2[0][1], $tri2[1][0], $tri2[1][1], $tri2[2][0], $tri2[2][1], $x, $y);
  echo rtrim('Point ' . _str($pt) . ' is within triangle ? ' . _str($ok)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $tri2 = [[1.0 / 10, 1.0 / 9], [100.0 / 8, 100.0 / 3], [-100.0 / 8, 100.0 / 6]];
  echo rtrim('Triangle is ' . _str($tri2)), PHP_EOL;
  $ok = pointInTriangle($tri2[0][0], $tri2[0][1], $tri2[1][0], $tri2[1][1], $tri2[2][0], $tri2[2][1], $x, $y);
  echo rtrim('Point ' . _str($pt) . ' is within triangle ? ' . _str($ok)), PHP_EOL;
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
