<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function zeros($n) {
  global $arr;
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, 0);
  $i = $i + 1;
};
  return $res;
}
function update(&$arr, $idx, $value) {
  $arr[$idx] = $value;
}
function query($arr, $left, $right) {
  $result = 0;
  $i = $left;
  while ($i < $right) {
  if ($arr[$i] > $result) {
  $result = $arr[$i];
}
  $i = $i + 1;
};
  return $result;
}
$arr = [0, 0, 0, 0, 0];
echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
update($arr, 4, 100);
echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
update($arr, 4, 0);
update($arr, 2, 20);
echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
update($arr, 4, 10);
echo rtrim(json_encode(query($arr, 2, 5), 1344)), PHP_EOL;
echo rtrim(json_encode(query($arr, 1, 5), 1344)), PHP_EOL;
update($arr, 2, 0);
echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
$arr = zeros(10000);
update($arr, 255, 30);
echo rtrim(json_encode(query($arr, 0, 10000), 1344)), PHP_EOL;
$arr = zeros(6);
update($arr, 5, 1);
echo rtrim(json_encode(query($arr, 5, 6), 1344)), PHP_EOL;
$arr = zeros(6);
update($arr, 0, 1000);
echo rtrim(json_encode(query($arr, 0, 1), 1344)), PHP_EOL;
