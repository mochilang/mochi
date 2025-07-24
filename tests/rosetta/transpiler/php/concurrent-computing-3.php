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
function mochi_shuffle($xs) {
  $arr = $xs;
  $i = count($arr) - 1;
  while ($i > 0) {
  $j = _now() % ($i + 1);
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  $i = $i - 1;
};
  return $arr;
}
$i = 0;
while ($i < 3) {
  echo rtrim(''), PHP_EOL;
  foreach (mochi_shuffle(['Enjoy', 'Rosetta', 'Code']) as $w) {
  echo rtrim($w), PHP_EOL;
};
  $i = $i + 1;
}
