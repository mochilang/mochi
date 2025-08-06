<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function permute($k, &$arr, $res) {
  if ($k == 1) {
  $copy = array_slice($arr, 0);
  return _append($res, $copy);
}
  $res = permute($k - 1, $arr, $res);
  $i = 0;
  while ($i < $k - 1) {
  if ($k % 2 == 0) {
  $temp = $arr[$i];
  $arr[$i] = $arr[$k - 1];
  $arr[$k - 1] = $temp;
} else {
  $temp = $arr[0];
  $arr[0] = $arr[$k - 1];
  $arr[$k - 1] = $temp;
}
  $res = permute($k - 1, $arr, $res);
  $i = $i + 1;
};
  return $res;
}
function heaps($arr) {
  if (count($arr) <= 1) {
  return [array_slice($arr, 0)];
}
  $res = [];
  $res = permute(count($arr), $arr, $res);
  return $res;
}
function main() {
  $perms = heaps([1, 2, 3]);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($perms, 1344))))))), PHP_EOL;
}
main();
