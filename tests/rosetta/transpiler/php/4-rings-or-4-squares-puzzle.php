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
  function validComb($a, $b, $c, $d, $e, $f, $g) {
  global $r1, $r2, $r3;
  $square1 = $a + $b;
  $square2 = $b + $c + $d;
  $square3 = $d + $e + $f;
  $square4 = $f + $g;
  return $square1 == $square2 && $square2 == $square3 && $square3 == $square4;
};
  function isUnique($a, $b, $c, $d, $e, $f, $g) {
  global $r1, $r2, $r3;
  $nums = [$a, $b, $c, $d, $e, $f, $g];
  $i = 0;
  while ($i < count($nums)) {
  $j = $i + 1;
  while ($j < count($nums)) {
  if ($nums[$i] == $nums[$j]) {
  return false;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return true;
};
  function getCombs($low, $high, $unique) {
  global $r1, $r2, $r3;
  $valid = [];
  $count = 0;
  for ($b = $low; $b < ($high + 1); $b++) {
  for ($c = $low; $c < ($high + 1); $c++) {
  for ($d = $low; $d < ($high + 1); $d++) {
  $s = $b + $c + $d;
  for ($e = $low; $e < ($high + 1); $e++) {
  for ($f = $low; $f < ($high + 1); $f++) {
  $a = $s - $b;
  $g = $s - $f;
  if ($a < $low || $a > $high) {
  continue;
}
  if ($g < $low || $g > $high) {
  continue;
}
  if ($d + $e + $f != $s) {
  continue;
}
  if ($f + $g != $s) {
  continue;
}
  if (!$unique || isUnique($a, $b, $c, $d, $e, $f, $g)) {
  $valid = array_merge($valid, [[$a, $b, $c, $d, $e, $f, $g]]);
  $count = $count + 1;
}
};
};
};
};
};
  return ['count' => $count, 'list' => $valid];
};
  $r1 = getCombs(1, 7, true);
  echo rtrim(_str($r1['count']) . ' unique solutions in 1 to 7'), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($r1['list'], 1344))))))), PHP_EOL;
  $r2 = getCombs(3, 9, true);
  echo rtrim(_str($r2['count']) . ' unique solutions in 3 to 9'), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($r2['list'], 1344))))))), PHP_EOL;
  $r3 = getCombs(0, 9, false);
  echo rtrim(_str($r3['count']) . ' non-unique solutions in 0 to 9'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
