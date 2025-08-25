<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function mochi_key($state, $obs) {
  global $emit_p, $observations, $result, $start_p, $states, $trans_p;
  return $state . '|' . $obs;
}
function viterbi($observations, $states, $start_p, $trans_p, $emit_p) {
  global $result;
  if (count($observations) == 0 || count($states) == 0) {
  _panic('empty parameters');
}
  $probs = [];
  $ptrs = [];
  $first_obs = $observations[0];
  $i = 0;
  while ($i < count($states)) {
  $state = $states[$i];
  $probs[mochi_key($state, $first_obs)] = $start_p[$state] * $emit_p[$state][$first_obs];
  $ptrs[mochi_key($state, $first_obs)] = '';
  $i = $i + 1;
};
  $t = 1;
  while ($t < count($observations)) {
  $obs = $observations[$t];
  $j = 0;
  while ($j < count($states)) {
  $state = $states[$j];
  $max_prob = -1.0;
  $prev_state = '';
  $k = 0;
  while ($k < count($states)) {
  $state0 = $states[$k];
  $obs0 = $observations[$t - 1];
  $prob_prev = $probs[mochi_key($state0, $obs0)];
  $prob = $prob_prev * $trans_p[$state0][$state] * $emit_p[$state][$obs];
  if ($prob > $max_prob) {
  $max_prob = $prob;
  $prev_state = $state0;
}
  $k = $k + 1;
};
  $probs[mochi_key($state, $obs)] = $max_prob;
  $ptrs[mochi_key($state, $obs)] = $prev_state;
  $j = $j + 1;
};
  $t = $t + 1;
};
  $path = [];
  $n = 0;
  while ($n < count($observations)) {
  $path = _append($path, '');
  $n = $n + 1;
};
  $last_obs = $observations[count($observations) - 1];
  $max_final = -1.0;
  $last_state = '';
  $m = 0;
  while ($m < count($states)) {
  $state = $states[$m];
  $prob = $probs[mochi_key($state, $last_obs)];
  if ($prob > $max_final) {
  $max_final = $prob;
  $last_state = $state;
}
  $m = $m + 1;
};
  $last_index = count($observations) - 1;
  $path[$last_index] = $last_state;
  $idx = $last_index;
  while ($idx > 0) {
  $obs = $observations[$idx];
  $prev = $ptrs[mochi_key($path[$idx], $obs)];
  $path[$idx - 1] = $prev;
  $idx = $idx - 1;
};
  return $path;
}
function join_words($words) {
  global $emit_p, $observations, $result, $start_p, $states, $trans_p;
  $res = '';
  $i = 0;
  while ($i < count($words)) {
  if ($i > 0) {
  $res = $res . ' ';
}
  $res = $res . $words[$i];
  $i = $i + 1;
};
  return $res;
}
$observations = ['normal', 'cold', 'dizzy'];
$states = ['Healthy', 'Fever'];
$start_p = ['Fever' => 0.4, 'Healthy' => 0.6];
$trans_p = ['Fever' => ['Fever' => 0.6, 'Healthy' => 0.4], 'Healthy' => ['Fever' => 0.3, 'Healthy' => 0.7]];
$emit_p = ['Fever' => ['cold' => 0.3, 'dizzy' => 0.6, 'normal' => 0.1], 'Healthy' => ['cold' => 0.4, 'dizzy' => 0.1, 'normal' => 0.5]];
$result = viterbi($observations, $states, $start_p, $trans_p, $emit_p);
echo rtrim(join_words($result)), PHP_EOL;
