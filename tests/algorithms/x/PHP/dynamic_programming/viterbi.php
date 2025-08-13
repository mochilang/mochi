<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_key($state, $obs) {
  global $emit_p, $observations, $result, $start_p, $states, $trans_p;
  return $state . '|' . $obs;
};
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
};
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
};
  $observations = ['normal', 'cold', 'dizzy'];
  $states = ['Healthy', 'Fever'];
  $start_p = ['Healthy' => 0.6, 'Fever' => 0.4];
  $trans_p = ['Healthy' => ['Healthy' => 0.7, 'Fever' => 0.3], 'Fever' => ['Healthy' => 0.4, 'Fever' => 0.6]];
  $emit_p = ['Healthy' => ['normal' => 0.5, 'cold' => 0.4, 'dizzy' => 0.1], 'Fever' => ['normal' => 0.1, 'cold' => 0.3, 'dizzy' => 0.6]];
  $result = viterbi($observations, $states, $start_p, $trans_p, $emit_p);
  echo rtrim(join_words($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
