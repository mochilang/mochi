<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function slice_list($arr, $start, $end) {
  global $arr_1, $nbf, $nrec, $nbf2, $nrec2, $nbf3, $nrec3;
  $res = [];
  $k = $start;
  while ($k < $end) {
  $res = _append($res, $arr[$k]);
  $k = $k + 1;
};
  return $res;
}
function count_inversions_bf($arr) {
  global $arr_1, $nbf, $nrec, $nbf2, $nrec2, $nbf3, $nrec3;
  $n = count($arr);
  $inv = 0;
  $i = 0;
  while ($i < $n - 1) {
  $j = $i + 1;
  while ($j < $n) {
  if ($arr[$i] > $arr[$j]) {
  $inv = $inv + 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $inv;
}
function count_cross_inversions($p, $q) {
  global $arr_1, $nbf, $nrec, $nbf2, $nrec2, $nbf3, $nrec3;
  $r = [];
  $i = 0;
  $j = 0;
  $inv = 0;
  while ($i < count($p) && $j < count($q)) {
  if ($p[$i] > $q[$j]) {
  $inv = $inv + (count($p) - $i);
  $r = _append($r, $q[$j]);
  $j = $j + 1;
} else {
  $r = _append($r, $p[$i]);
  $i = $i + 1;
}
};
  if ($i < count($p)) {
  $r = array_merge($r, slice_list($p, $i, count($p)));
} else {
  $r = array_merge($r, slice_list($q, $j, count($q)));
}
  return ['arr' => $r, 'inv' => $inv];
}
function count_inversions_recursive($arr) {
  global $arr_1, $nbf, $nrec, $nbf2, $nrec2, $nbf3, $nrec3;
  if (count($arr) <= 1) {
  return ['arr' => $arr, 'inv' => 0];
}
  $mid = count($arr) / 2;
  $p = slice_list($arr, 0, $mid);
  $q = slice_list($arr, $mid, count($arr));
  $res_p = count_inversions_recursive($p);
  $res_q = count_inversions_recursive($q);
  $res_cross = count_cross_inversions($res_p['arr'], $res_q['arr']);
  $total = $res_p['inv'] + $res_q['inv'] + $res_cross['inv'];
  return ['arr' => $res_cross['arr'], 'inv' => $total];
}
$arr_1 = [10, 2, 1, 5, 5, 2, 11];
$nbf = count_inversions_bf($arr_1);
$nrec = count_inversions_recursive($arr_1)['inv'];
echo rtrim('number of inversions = ') . " " . rtrim(json_encode($nbf, 1344)), PHP_EOL;
$arr_1 = [1, 2, 2, 5, 5, 10, 11];
$nbf2 = count_inversions_bf($arr_1);
$nrec2 = count_inversions_recursive($arr_1)['inv'];
echo rtrim('number of inversions = ') . " " . rtrim(json_encode($nbf2, 1344)), PHP_EOL;
$arr_1 = [];
$nbf3 = count_inversions_bf($arr_1);
$nrec3 = count_inversions_recursive($arr_1)['inv'];
echo rtrim('number of inversions = ') . " " . rtrim(json_encode($nbf3, 1344)), PHP_EOL;
