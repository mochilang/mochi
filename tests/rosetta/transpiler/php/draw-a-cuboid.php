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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_repeat($ch, $n) {
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
};
  function cubLine($n, $dx, $dy, $cde) {
  $line = repeat(' ', $n + 1) . substr($cde, 0, 1 - 0);
  $d = 9 * $dx - 1;
  while ($d > 0) {
  $line = $line . substr($cde, 1, 2 - 1);
  $d = $d - 1;
};
  $line = $line . substr($cde, 0, 1 - 0);
  $line = $line . repeat(' ', $dy) . substr($cde, 2);
  echo rtrim($line), PHP_EOL;
};
  function cuboid($dx, $dy, $dz) {
  echo rtrim('cuboid ' . _str($dx) . ' ' . _str($dy) . ' ' . _str($dz) . ':'), PHP_EOL;
  cubLine($dy + 1, $dx, 0, '+-');
  $i = 1;
  while ($i <= $dy) {
  cubLine($dy - $i + 1, $dx, $i - 1, '/ |');
  $i = $i + 1;
};
  cubLine(0, $dx, $dy, '+-|');
  $j = 4 * $dz - $dy - 2;
  while ($j > 0) {
  cubLine(0, $dx, $dy, '| |');
  $j = $j - 1;
};
  cubLine(0, $dx, $dy, '| +');
  $i = 1;
  while ($i <= $dy) {
  cubLine(0, $dx, $dy - $i, '| /');
  $i = $i + 1;
};
  cubLine(0, $dx, 0, '+-
');
};
  cuboid(2, 3, 4);
  echo rtrim(''), PHP_EOL;
  cuboid(1, 1, 1);
  echo rtrim(''), PHP_EOL;
  cuboid(6, 2, 1);
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
