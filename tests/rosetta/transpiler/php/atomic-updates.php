<?php
function randOrder($seed, $n) {
  global $randChaos, $main;
  $next = ($seed * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
}
function randChaos($seed, $n) {
  global $randOrder, $main;
  $next = ($seed * 1103515245 + 12345) % 2147483647;
  return [$next, $next % $n];
}
function main() {
  global $randOrder, $randChaos;
  $nBuckets = 10;
  $initialSum = 1000;
  $buckets = [];
  for ($i = 0; $i < $nBuckets; $i++) {
  $buckets = array_merge($buckets, [0]);
};
  $i = $nBuckets;
  $dist = $initialSum;
  while ($i > 0) {
  $v = intdiv($dist, $i);
  $i = $i - 1;
  $buckets[$i] = $v;
  $dist = $dist - $v;
};
  $tc0 = 0;
  $tc1 = 0;
  $total = 0;
  $nTicks = 0;
  $seedOrder = 1;
  $seedChaos = 2;
  echo "sum  ---updates---    mean  buckets", PHP_EOL;
  $t = 0;
  while ($t < 5) {
  $r = randOrder($seedOrder, $nBuckets);
  $seedOrder = $r[0];
  $b1 = $r[1];
  $b2 = ($b1 + 1) % $nBuckets;
  $v1 = $buckets[$b1];
  $v2 = $buckets[$b2];
  if ($v1 > $v2) {
  $a = intval((($v1 - $v2) / 2));
  if ($a > $buckets[$b1]) {
  $a = $buckets[$b1];
};
  $buckets[$b1] = $buckets[$b1] - $a;
  $buckets[$b2] = $buckets[$b2] + $a;
} else {
  $a = intval((($v2 - $v1) / 2));
  if ($a > $buckets[$b2]) {
  $a = $buckets[$b2];
};
  $buckets[$b2] = $buckets[$b2] - $a;
  $buckets[$b1] = $buckets[$b1] + $a;
}
  $tc0 = $tc0 + 1;
  $r = randChaos($seedChaos, $nBuckets);
  $seedChaos = $r[0];
  $b1 = $r[1];
  $b2 = ($b1 + 1) % $nBuckets;
  $r = randChaos($seedChaos, $buckets[$b1] + 1);
  $seedChaos = $r[0];
  $amt = $r[1];
  if ($amt > $buckets[$b1]) {
  $amt = $buckets[$b1];
}
  $buckets[$b1] = $buckets[$b1] - $amt;
  $buckets[$b2] = $buckets[$b2] + $amt;
  $tc1 = $tc1 + 1;
  $sum = 0;
  $idx = 0;
  while ($idx < $nBuckets) {
  $sum = $sum + $buckets[$idx];
  $idx = $idx + 1;
};
  $total = $total + $tc0 + $tc1;
  $nTicks = $nTicks + 1;
  echo json_encode($sum, 1344) . " " . json_encode($tc0, 1344) . " " . json_encode($tc1, 1344) . " " . json_encode(intdiv($total, $nTicks), 1344) . "  " . json_encode($buckets, 1344), PHP_EOL;
  $tc0 = 0;
  $tc1 = 0;
  $t = $t + 1;
};
}
main();
