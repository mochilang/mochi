<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function insertion_sort(&$a) {
  $i = 1;
  while ($i < count($a)) {
  $key = $a[$i];
  $j = $i - 1;
  while ($j >= 0 && $a[$j] > $key) {
  $a[$j + 1] = $a[$j];
  $j = $j - 1;
};
  $a[$j + 1] = $key;
  $i = $i + 1;
};
  return $a;
}
function median_filter($gray_img, $mask) {
  $rows = count($gray_img);
  $cols = count($gray_img[0]);
  $bd = _intdiv($mask, 2);
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  $i = $bd;
  while ($i < $rows - $bd) {
  $j = $bd;
  while ($j < $cols - $bd) {
  $kernel = [];
  $x = $i - $bd;
  while ($x <= $i + $bd) {
  $y = $j - $bd;
  while ($y <= $j + $bd) {
  $kernel = _append($kernel, $gray_img[$x][$y]);
  $y = $y + 1;
};
  $x = $x + 1;
};
  $kernel = insertion_sort($kernel);
  $idx = _intdiv(($mask * $mask), 2);
  $result[$i][$j] = $kernel[$idx];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $result;
}
function main() {
  $img = [[10, 10, 10, 10, 10], [10, 255, 10, 255, 10], [10, 10, 10, 10, 10], [10, 255, 10, 255, 10], [10, 10, 10, 10, 10]];
  $filtered = median_filter($img, 3);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($filtered, 1344))))))), PHP_EOL;
}
main();
