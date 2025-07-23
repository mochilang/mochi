<?php
function neighborsList() {
  global $plus, $isStable, $topple, $pileString, $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  return [[1, 3], [0, 2, 4], [1, 5], [0, 4, 6], [1, 3, 5, 7], [2, 4, 8], [3, 7], [4, 6, 8], [5, 7]];
}
function plus(&$a, &$b) {
  global $neighborsList, $isStable, $topple, $pileString, $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = array_merge($res, [$a[$i] + $b[$i]]);
  $i = $i + 1;
};
  return $res;
}
function isStable(&$p) {
  global $neighborsList, $plus, $topple, $pileString, $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  foreach ($p as $v) {
  if ($v > 3) {
  return false;
}
};
  return true;
}
function topple(&$p) {
  global $neighborsList, $plus, $isStable, $pileString, $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  $neighbors = neighborsList();
  $i = 0;
  while ($i < count($p)) {
  if ($p[$i] > 3) {
  $p[$i] = $p[$i] - 4;
  $nbs = $neighbors[$i];
  foreach ($nbs as $j) {
  $p[$j] = $p[$j] + 1;
};
  return 0;
}
  $i = $i + 1;
};
  return 0;
}
function pileString(&$p) {
  global $neighborsList, $plus, $isStable, $topple, $s4, $s1, $s2, $s3_a, $s3_b, $s3, $s3_id, $s4b, $s5;
  $s = "";
  $r = 0;
  while ($r < 3) {
  $c = 0;
  while ($c < 3) {
  $s = $s . json_encode($p[3 * $r + $c], 1344) . " ";
  $c = $c + 1;
};
  $s = $s . "\n";
  $r = $r + 1;
};
  return $s;
}
echo "Avalanche of topplings:\n", PHP_EOL;
$s4 = [4, 3, 3, 3, 1, 2, 0, 2, 3];
echo pileString($s4), PHP_EOL;
while (!isStable($s4)) {
  topple($s4);
  echo pileString($s4), PHP_EOL;
}
echo "Commutative additions:\n", PHP_EOL;
$s1 = [1, 2, 0, 2, 1, 1, 0, 1, 3];
$s2 = [2, 1, 3, 1, 0, 1, 0, 1, 0];
$s3_a = plus($s1, $s2);
while (!isStable($s3_a)) {
  topple($s3_a);
}
$s3_b = plus($s2, $s1);
while (!isStable($s3_b)) {
  topple($s3_b);
}
echo pileString($s1) . "\nplus\n\n" . pileString($s2) . "\nequals\n\n" . pileString($s3_a), PHP_EOL;
echo "and\n\n" . pileString($s2) . "\nplus\n\n" . pileString($s1) . "\nalso equals\n\n" . pileString($s3_b), PHP_EOL;
echo "Addition of identity sandpile:\n", PHP_EOL;
$s3 = [3, 3, 3, 3, 3, 3, 3, 3, 3];
$s3_id = [2, 1, 2, 1, 0, 1, 2, 1, 2];
$s4b = plus($s3, $s3_id);
while (!isStable($s4b)) {
  topple($s4b);
}
echo pileString($s3) . "\nplus\n\n" . pileString($s3_id) . "\nequals\n\n" . pileString($s4b), PHP_EOL;
echo "Addition of identities:\n", PHP_EOL;
$s5 = plus($s3_id, $s3_id);
while (!isStable($s5)) {
  topple($s5);
}
echo pileString($s3_id) . "\nplus\n\n" . pileString($s3_id) . "\nequals\n\n" . pileString($s5), PHP_EOL;
