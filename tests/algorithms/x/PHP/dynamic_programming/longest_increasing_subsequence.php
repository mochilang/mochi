<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function longest_subsequence($xs) {
  $n = count($xs);
  if ($n <= 1) {
  return $xs;
}
  $pivot = $xs[0];
  $is_found = false;
  $i = 1;
  $longest_subseq = [];
  while (!$is_found && $i < $n) {
  if ($xs[$i] < $pivot) {
  $is_found = true;
  $temp_array = array_slice($xs, $i, $n - $i);
  $temp_array = longest_subsequence($temp_array);
  if (_len($temp_array) > count($longest_subseq)) {
  $longest_subseq = $temp_array;
};
} else {
  $i = $i + 1;
}
};
  $filtered = [];
  $j = 1;
  while ($j < $n) {
  if ($xs[$j] >= $pivot) {
  $filtered = _append($filtered, $xs[$j]);
}
  $j = $j + 1;
};
  $candidate = [];
  $candidate = _append($candidate, $pivot);
  $candidate = array_merge($candidate, longest_subsequence($filtered));
  if (count($candidate) > count($longest_subseq)) {
  return $candidate;
} else {
  return $longest_subseq;
}
}
