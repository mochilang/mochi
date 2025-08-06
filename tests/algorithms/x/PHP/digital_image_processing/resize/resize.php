<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function zeros3d($h, $w, $c) {
  $arr = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $pixel = [];
  $k = 0;
  while ($k < $c) {
  $pixel = _append($pixel, 0);
  $k = $k + 1;
};
  $row = _append($row, $pixel);
  $x = $x + 1;
};
  $arr = _append($arr, $row);
  $y = $y + 1;
};
  return $arr;
}
function resize_nn($img, $dst_w, $dst_h) {
  $src_h = count($img);
  $src_w = count($img[0]);
  $channels = count($img[0][0]);
  $ratio_x = (floatval($src_w)) / (floatval($dst_w));
  $ratio_y = (floatval($src_h)) / (floatval($dst_h));
  $out = zeros3d($dst_h, $dst_w, $channels);
  $i = 0;
  while ($i < $dst_h) {
  $j = 0;
  while ($j < $dst_w) {
  $src_x = intval(($ratio_x * (floatval($j))));
  $src_y = intval(($ratio_y * (floatval($i))));
  $out[$i][$j] = $img[$src_y][$src_x];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $out;
}
function main() {
  $img = [[[0, 0, 0], [255, 255, 255]], [[255, 0, 0], [0, 255, 0]]];
  $resized = resize_nn($img, 4, 4);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($resized, 1344))))))), PHP_EOL;
}
main();
