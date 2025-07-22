<?php
function mochi_shuffle($xs) {
  global $doTrials, $main;
  $arr = $xs;
  $i = 99;
  while ($i > 0) {
  $j = hrtime(true) % ($i + 1);
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
  $n = hrtime(true) % 100;
  while ($opened[$n]) {
  $n = hrtime(true) % 100;
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
  echo "  strategy = " . $strategy . "  pardoned = " . strval($pardoned) . " relative frequency = " . strval($rf) . "%", PHP_EOL;
}
function main() {
  global $mochi_shuffle, $doTrials;
  $trials = 1000;
  foreach ([10, 100] as $np) {
  echo "Results from " . strval($trials) . " trials with " . strval($np) . " prisoners:\n", PHP_EOL;
  foreach (["random", "optimal"] as $strat) {
  doTrials($trials, $np, $strat);
};
};
}
main();
