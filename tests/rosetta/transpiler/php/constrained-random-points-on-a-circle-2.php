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
$nPts = 100;
$rMin = 10;
$rMax = 15;
$span = $rMax + 1 + $rMax;
$poss = [];
$min2 = $rMin * $rMin;
$max2 = $rMax * $rMax;
$y = -$rMax;
while ($y <= $rMax) {
  $x = -$rMax;
  while ($x <= $rMax) {
  $r2 = $x * $x + $y * $y;
  if ($r2 >= $min2 && $r2 <= $max2) {
  $poss = array_merge($poss, [[$x, $y]]);
}
  $x = $x + 1;
};
  $y = $y + 1;
}
echo rtrim(_str(count($poss)) . ' possible points'), PHP_EOL;
$rows = [];
$r = 0;
while ($r < $span) {
  $row = [];
  $c = 0;
  while ($c < $span * 2) {
  $row = array_merge($row, [' ']);
  $c = $c + 1;
};
  $rows = array_merge($rows, [$row]);
  $r = $r + 1;
}
$u = 0;
$seen = [];
$n = 0;
while ($n < $nPts) {
  $i = _now() % count($poss);
  $x = $poss[$i][0];
  $yy = $poss[$i][1];
  $row = $yy + $rMax;
  $col = ($x + $rMax) * 2;
  $rows[$row][$col] = '*';
  $key = _str($row) . ',' . _str($col);
  if (!$seen[$key]) {
  $seen[$key] = true;
  $u = $u + 1;
}
  $n = $n + 1;
}
$i2 = 0;
while ($i2 < $span) {
  $line = '';
  $j = 0;
  while ($j < $span * 2) {
  $line = $line . $rows[$i2][$j];
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i2 = $i2 + 1;
}
echo rtrim(_str($u) . ' unique points'), PHP_EOL;
