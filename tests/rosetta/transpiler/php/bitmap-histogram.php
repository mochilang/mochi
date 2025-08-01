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
  function image() {
  return [[0, 0, 10000], [65535, 65535, 65535], [65535, 65535, 65535]];
};
  function histogram($g, $bins) {
  if ($bins <= 0) {
  $bins = count($g[0]);
}
  $h = [];
  $i = 0;
  while ($i < $bins) {
  $h = array_merge($h, [0]);
  $i = $i + 1;
};
  $y = 0;
  while ($y < count($g)) {
  $row = $g[$y];
  $x = 0;
  while ($x < count($row)) {
  $p = $row[$x];
  $idx = intval((_intdiv(($p * ($bins - 1)), 65535)));
  $h[$idx] = $h[$idx] + 1;
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $h;
};
  function medianThreshold($h) {
  $lb = 0;
  $ub = count($h) - 1;
  $lSum = 0;
  $uSum = 0;
  while ($lb <= $ub) {
  if ($lSum + $h[$lb] < $uSum + $h[$ub]) {
  $lSum = $lSum + $h[$lb];
  $lb = $lb + 1;
} else {
  $uSum = $uSum + $h[$ub];
  $ub = $ub - 1;
}
};
  return intval((($ub * 65535) / count($h)));
};
  function threshold($g, $t) {
  $out = [];
  $y = 0;
  while ($y < count($g)) {
  $row = $g[$y];
  $newRow = [];
  $x = 0;
  while ($x < count($row)) {
  if ($row[$x] < $t) {
  $newRow = array_merge($newRow, [0]);
} else {
  $newRow = array_merge($newRow, [65535]);
}
  $x = $x + 1;
};
  $out = array_merge($out, [$newRow]);
  $y = $y + 1;
};
  return $out;
};
  function printImage($g) {
  $y = 0;
  while ($y < count($g)) {
  $row = $g[$y];
  $line = '';
  $x = 0;
  while ($x < count($row)) {
  if ($row[$x] == 0) {
  $line = $line . '0';
} else {
  $line = $line . '1';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
};
  function main() {
  $img = image();
  $h = histogram($img, 0);
  echo rtrim('Histogram: ' . _str($h)), PHP_EOL;
  $t = medianThreshold($h);
  echo rtrim('Threshold: ' . _str($t)), PHP_EOL;
  $bw = threshold($img, $t);
  printImage($bw);
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
