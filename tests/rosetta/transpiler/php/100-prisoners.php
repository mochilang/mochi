<?php
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
  global $doTrials, $main;
  $arr = $xs;
  $i = 99;
  while ($i > 0) {
  $j = _now() % ($i + 1);
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  $i = $i - 1;
};
  return $arr;
}
function doTrials($trials, $np, $strategy) {
  global $mochi_shuffle, $main;
  $pardoned = 0;
  $t = 0;
  while ($t < $trials) {
  $drawers = [];
  $i = 0;
  while ($i < 100) {
  $drawers = array_merge($drawers, [$i]);
  $i = $i + 1;
};
  $drawers = mochi_shuffle($drawers);
  $p = 0;
  $success = true;
  while ($p < $np) {
  $found = false;
  if ($strategy == "optimal") {
  $prev = $p;
  $d = 0;
  while ($d < 50) {
  $mochi_this = $drawers[$prev];
  if ($mochi_this == $p) {
  $found = true;
  break;
}
  $prev = $mochi_this;
  $d = $d + 1;
};
} else {
  $opened = [];
  $k = 0;
  while ($k < 100) {
  $opened = array_merge($opened, [false]);
  $k = $k + 1;
};
  $d = 0;
  while ($d < 50) {
  $n = _now() % 100;
  while ($opened[$n]) {
  $n = _now() % 100;
};
  $opened[$n] = true;
  if ($drawers[$n] == $p) {
  $found = true;
  break;
}
  $d = $d + 1;
};
}
  if (!$found) {
  $success = false;
  break;
}
  $p = $p + 1;
};
  if ($success) {
  $pardoned = $pardoned + 1;
}
  $t = $t + 1;
};
  $rf = ($pardoned) / ($trials) * 100.0;
  echo "  strategy = " . $strategy . "  pardoned = " . json_encode($pardoned, 1344) . " relative frequency = " . json_encode($rf, 1344) . "%", PHP_EOL;
}
function main() {
  global $mochi_shuffle, $doTrials;
  $trials = 1000;
  foreach ([10, 100] as $np) {
  echo "Results from " . json_encode($trials, 1344) . " trials with " . json_encode($np, 1344) . " prisoners:\n", PHP_EOL;
  foreach (["random", "optimal"] as $strat) {
  doTrials($trials, $np, $strat);
};
};
}
main();
