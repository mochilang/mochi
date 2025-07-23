<?php
function sortRunes($s) {
  global $deranged, $main;
  $arr = [];
  $i = 0;
  while ($i < strlen($s)) {
  $arr = array_merge($arr, [substr($s, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $n = count($arr);
  $m = 0;
  while ($m < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $m = $m + 1;
};
  $out = "";
  $i = 0;
  while ($i < $n) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return $out;
}
function deranged($a, $b) {
  global $sortRunes, $main;
  if (strlen($a) != strlen($b)) {
  return false;
}
  $i = 0;
  while ($i < strlen($a)) {
  if (substr($a, $i, $i + 1 - $i) == substr($b, $i, $i + 1 - $i)) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function main() {
  global $sortRunes, $deranged;
  $words = ["constitutionalism", "misconstitutional"];
  $m = [];
  $bestLen = 0;
  $w1 = "";
  $w2 = "";
  foreach ($words as $w) {
  if (strlen($w) <= $bestLen) {
  continue;
}
  $k = sortRunes($w);
  if (!(array_key_exists($k, $m))) {
  $m[$k] = [$w];
  continue;
}
  foreach ($m[$k] as $c) {
  if (deranged($w, $c)) {
  $bestLen = strlen($w);
  $w1 = $c;
  $w2 = $w;
  break;
}
};
  $m[$k] = array_merge($m[$k], [$w]);
};
  echo $w1 . " " . $w2 . " : Length " . json_encode($bestLen, 1344), PHP_EOL;
}
main();
