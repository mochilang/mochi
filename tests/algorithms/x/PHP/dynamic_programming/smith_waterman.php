<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function score_function($source_char, $target_char, $match_score, $mismatch_score, $gap_score) {
  global $query, $subject, $score;
  if ($source_char == '-' || $target_char == '-') {
  return $gap_score;
}
  if ($source_char == $target_char) {
  return $match_score;
}
  return $mismatch_score;
}
function smith_waterman($query, $subject, $match_score, $mismatch_score, $gap_score) {
  $q = strtoupper($query);
  $s = strtoupper($subject);
  $m = strlen($q);
  $n = strlen($s);
  $score = [];
  for ($_ = 0; $_ < ($m + 1); $_++) {
  $row = [];
  for ($_2 = 0; $_2 < ($n + 1); $_2++) {
  $row = _append($row, 0);
};
  $score = _append($score, $row);
};
  for ($i = 1; $i < ($m + 1); $i++) {
  for ($j = 1; $j < ($n + 1); $j++) {
  $qc = substr($q, $i - 1, $i - ($i - 1));
  $sc = substr($s, $j - 1, $j - ($j - 1));
  $diag = $score[$i - 1][$j - 1] + score_function($qc, $sc, $match_score, $mismatch_score, $gap_score);
  $delete = $score[$i - 1][$j] + $gap_score;
  $insert = $score[$i][$j - 1] + $gap_score;
  $max_val = 0;
  if ($diag > $max_val) {
  $max_val = $diag;
}
  if ($delete > $max_val) {
  $max_val = $delete;
}
  if ($insert > $max_val) {
  $max_val = $insert;
}
  $score[$i][$j] = $max_val;
};
};
  return $score;
}
function traceback($score, $query, $subject, $match_score, $mismatch_score, $gap_score) {
  $q = strtoupper($query);
  $s = strtoupper($subject);
  $max_value = 0;
  $i_max = 0;
  $j_max = 0;
  for ($i = 0; $i < count($score); $i++) {
  for ($j = 0; $j < count($score[$i]); $j++) {
  if ($score[$i][$j] > $max_value) {
  $max_value = $score[$i][$j];
  $i_max = $i;
  $j_max = $j;
}
};
};
  $i = $i_max;
  $j = $j_max;
  $align1 = '';
  $align2 = '';
  $gap_penalty = score_function('-', '-', $match_score, $mismatch_score, $gap_score);
  if ($i == 0 || $j == 0) {
  return '';
}
  while ($i > 0 && $j > 0) {
  $qc = substr($q, $i - 1, $i - ($i - 1));
  $sc = substr($s, $j - 1, $j - ($j - 1));
  if ($score[$i][$j] == $score[$i - 1][$j - 1] + score_function($qc, $sc, $match_score, $mismatch_score, $gap_score)) {
  $align1 = $qc . $align1;
  $align2 = $sc . $align2;
  $i = $i - 1;
  $j = $j - 1;
} else {
  if ($score[$i][$j] == $score[$i - 1][$j] + $gap_penalty) {
  $align1 = $qc . $align1;
  $align2 = '-' . $align2;
  $i = $i - 1;
} else {
  $align1 = '-' . $align1;
  $align2 = $sc . $align2;
  $j = $j - 1;
};
}
};
  return $align1 . '
' . $align2;
}
$query = 'HEAGAWGHEE';
$subject = 'PAWHEAE';
$score = smith_waterman($query, $subject, 1, -1, -2);
echo rtrim(traceback($score, $query, $subject, 1, -1, -2)), PHP_EOL;
