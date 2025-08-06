<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function convert_to_negative($img) {
  $result = [];
  $i = 0;
  while ($i < count($img)) {
  $row = [];
  $j = 0;
  while ($j < count($img[$i])) {
  $pixel = $img[$i][$j];
  $r = 255 - $pixel[0];
  $g = 255 - $pixel[1];
  $b = 255 - $pixel[2];
  $row = _append($row, [$r, $g, $b]);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function main() {
  $image = [[[10, 20, 30], [0, 0, 0]], [[255, 255, 255], [100, 150, 200]]];
  $neg = convert_to_negative($image);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($neg, 1344))))))), PHP_EOL;
}
main();
