<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function clamp($value) {
  global $sample;
  if ($value < 0) {
  return 0;
}
  if ($value > 255) {
  return 255;
}
  return $value;
}
function change_brightness($img, $level) {
  global $sample;
  if ($level < (-255) || $level > 255) {
  $panic('level must be between -255 and 255');
}
  $result = [];
  $i = 0;
  while ($i < count($img)) {
  $row_res = [];
  $j = 0;
  while ($j < count($img[$i])) {
  $row_res = _append($row_res, clamp($img[$i][$j] + $level));
  $j = $j + 1;
};
  $result = _append($result, $row_res);
  $i = $i + 1;
};
  return $result;
}
$sample = [[100, 150], [200, 250]];
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(change_brightness($sample, 30), 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(change_brightness($sample, -60), 1344))))))), PHP_EOL;
