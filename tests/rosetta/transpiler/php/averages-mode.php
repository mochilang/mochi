<?php
ini_set('memory_limit', '-1');
$arr1 = [2, 7, 1, 8, 2];
$counts1 = [];
$keys1 = [];
$i = 0;
while ($i < count($arr1)) {
  $v = $arr1[$i];
  if (array_key_exists($v, $counts1)) {
  $counts1[$v] = $counts1[$v] + 1;
} else {
  $counts1[$v] = 1;
  $keys1 = array_merge($keys1, [$v]);
}
  $i = $i + 1;
}
$max1 = 0;
$i = 0;
while ($i < count($keys1)) {
  $k = $keys1[$i];
  $c = $counts1[$k];
  if ($c > $max1) {
  $max1 = $c;
}
  $i = $i + 1;
}
$modes1 = [];
$i = 0;
while ($i < count($keys1)) {
  $k = $keys1[$i];
  if ($counts1[$k] == $max1) {
  $modes1 = array_merge($modes1, [$k]);
}
  $i = $i + 1;
}
echo rtrim(json_encode($modes1, 1344)), PHP_EOL;
$arr2 = [2, 7, 1, 8, 2, 8];
$counts2 = [];
$keys2 = [];
$i = 0;
while ($i < count($arr2)) {
  $v = $arr2[$i];
  if (array_key_exists($v, $counts2)) {
  $counts2[$v] = $counts2[$v] + 1;
} else {
  $counts2[$v] = 1;
  $keys2 = array_merge($keys2, [$v]);
}
  $i = $i + 1;
}
$max2 = 0;
$i = 0;
while ($i < count($keys2)) {
  $k = $keys2[$i];
  $c = $counts2[$k];
  if ($c > $max2) {
  $max2 = $c;
}
  $i = $i + 1;
}
$modes2 = [];
$i = 0;
while ($i < count($keys2)) {
  $k = $keys2[$i];
  if ($counts2[$k] == $max2) {
  $modes2 = array_merge($modes2, [$k]);
}
  $i = $i + 1;
}
echo rtrim(json_encode($modes2, 1344)), PHP_EOL;
