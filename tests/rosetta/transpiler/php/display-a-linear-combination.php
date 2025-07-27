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
  function padRight($s, $w) {
  $r = $s;
  while (strlen($r) < $w) {
  $r = $r . ' ';
};
  return $r;
};
  function linearCombo($c) {
  $out = '';
  $i = 0;
  while ($i < count($c)) {
  $n = $c[$i];
  if ($n != 0) {
  $op = '';
  if ($n < 0 && strlen($out) == 0) {
  $op = '-';
} else {
  if ($n < 0) {
  $op = ' - ';
} else {
  if ($n > 0 && strlen($out) == 0) {
  $op = '';
} else {
  $op = ' + ';
};
};
};
  $av = $n;
  if ($av < 0) {
  $av = -$av;
};
  $coeff = _str($av) . '*';
  if ($av == 1) {
  $coeff = '';
};
  $out = $out . $op . $coeff . 'e(' . _str($i + 1) . ')';
}
  $i = $i + 1;
};
  if (strlen($out) == 0) {
  return '0';
}
  return $out;
};
  function main() {
  $combos = [[1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0], [0, 0, 0], [0], [1, 1, 1], [-1, -1, -1], [-1, -2, 0, -3], [-1]];
  $idx = 0;
  while ($idx < count($combos)) {
  $c = $combos[$idx];
  $t = '[';
  $j = 0;
  while ($j < count($c)) {
  $t = $t . _str($c[$j]);
  if ($j < count($c) - 1) {
  $t = $t . ', ';
}
  $j = $j + 1;
};
  $t = $t . ']';
  $lc = linearCombo($c);
  echo rtrim(padRight($t, 15) . '  ->  ' . $lc), PHP_EOL;
  $idx = $idx + 1;
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
