<?php
ini_set('memory_limit', '-1');
function bsearch($arr, $x) {
  global $bsearchRec, $main;
  $low = 0;
  $high = count($arr) - 1;
  while ($low <= $high) {
  $mid = intdiv(($low + $high), 2);
  if ($arr[$mid] > $x) {
  $high = $mid - 1;
} else {
  if ($arr[$mid] < $x) {
  $low = $mid + 1;
} else {
  return $mid;
};
}
};
  return -1;
}
function bsearchRec($arr, $x, $low, $high) {
  global $bsearch, $main;
  if ($high < $low) {
  return -1;
}
  $mid = intdiv(($low + $high), 2);
  if ($arr[$mid] > $x) {
  return bsearchRec($arr, $x, $low, $mid - 1);
} else {
  if ($arr[$mid] < $x) {
  return bsearchRec($arr, $x, $mid + 1, $high);
};
}
  return $mid;
}
function main() {
  global $bsearch, $bsearchRec;
  $nums = [-31, 0, 1, 2, 2, 4, 65, 83, 99, 782];
  $x = 2;
  $idx = bsearch($nums, $x);
  if ($idx >= 0) {
  echo rtrim(json_encode($x, 1344) . ' is at index ' . json_encode($idx, 1344) . '.'), PHP_EOL;
} else {
  echo rtrim(json_encode($x, 1344) . ' is not found.'), PHP_EOL;
}
  $x = 5;
  $idx = bsearchRec($nums, $x, 0, count($nums) - 1);
  if ($idx >= 0) {
  echo rtrim(json_encode($x, 1344) . ' is at index ' . json_encode($idx, 1344) . '.'), PHP_EOL;
} else {
  echo rtrim(json_encode($x, 1344) . ' is not found.'), PHP_EOL;
}
}
main();
