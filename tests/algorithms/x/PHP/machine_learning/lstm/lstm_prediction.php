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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function exp_approx($x) {
  global $data, $look_back, $epochs, $lr, $w, $test_seq, $pred;
  $sum = 1.0;
  $term = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function sigmoid($x) {
  global $data, $look_back, $epochs, $lr, $w, $test_seq, $pred;
  return 1.0 / (1.0 + exp_approx(-$x));
};
  function tanh_approx($x) {
  global $data, $look_back, $epochs, $lr, $w, $test_seq, $pred;
  $e = exp_approx(2.0 * $x);
  return ($e - 1.0) / ($e + 1.0);
};
  function forward($seq, $w) {
  global $data, $look_back, $epochs, $lr, $test_seq, $pred;
  $i_arr = [];
  $f_arr = [];
  $o_arr = [];
  $g_arr = [];
  $c_arr = [0.0];
  $h_arr = [0.0];
  $t = 0;
  while ($t < count($seq)) {
  $x = $seq[$t];
  $h_prev = $h_arr[$t];
  $c_prev = $c_arr[$t];
  $i_t = sigmoid($w['w_i'] * $x + $w['u_i'] * $h_prev + $w['b_i']);
  $f_t = sigmoid($w['w_f'] * $x + $w['u_f'] * $h_prev + $w['b_f']);
  $o_t = sigmoid($w['w_o'] * $x + $w['u_o'] * $h_prev + $w['b_o']);
  $g_t = tanh_approx($w['w_c'] * $x + $w['u_c'] * $h_prev + $w['b_c']);
  $c_t = $f_t * $c_prev + $i_t * $g_t;
  $h_t = $o_t * tanh_approx($c_t);
  $i_arr = _append($i_arr, $i_t);
  $f_arr = _append($f_arr, $f_t);
  $o_arr = _append($o_arr, $o_t);
  $g_arr = _append($g_arr, $g_t);
  $c_arr = _append($c_arr, $c_t);
  $h_arr = _append($h_arr, $h_t);
  $t = $t + 1;
};
  return ['i' => $i_arr, 'f' => $f_arr, 'o' => $o_arr, 'g' => $g_arr, 'c' => $c_arr, 'h' => $h_arr];
};
  function backward($seq, $target, &$w, $s, $lr) {
  global $data, $look_back, $epochs, $test_seq, $pred;
  $dw_i = 0.0;
  $du_i = 0.0;
  $db_i = 0.0;
  $dw_f = 0.0;
  $du_f = 0.0;
  $db_f = 0.0;
  $dw_o = 0.0;
  $du_o = 0.0;
  $db_o = 0.0;
  $dw_c = 0.0;
  $du_c = 0.0;
  $db_c = 0.0;
  $dw_y = 0.0;
  $db_y = 0.0;
  $T = count($seq);
  $h_last = $s['h'][$T];
  $y = $w['w_y'] * $h_last + $w['b_y'];
  $dy = $y - $target;
  $dw_y = $dy * $h_last;
  $db_y = $dy;
  $dh_next = $dy * $w['w_y'];
  $dc_next = 0.0;
  $t = $T - 1;
  while ($t >= 0) {
  $i_t = $s['i'][$t];
  $f_t = $s['f'][$t];
  $o_t = $s['o'][$t];
  $g_t = $s['g'][$t];
  $c_t = $s['c'][$t + 1];
  $c_prev = $s['c'][$t];
  $h_prev = $s['h'][$t];
  $tanh_c = tanh_approx($c_t);
  $do_t = $dh_next * $tanh_c;
  $da_o = $do_t * $o_t * (1.0 - $o_t);
  $dc = $dh_next * $o_t * (1.0 - $tanh_c * $tanh_c) + $dc_next;
  $di_t = $dc * $g_t;
  $da_i = $di_t * $i_t * (1.0 - $i_t);
  $dg_t = $dc * $i_t;
  $da_g = $dg_t * (1.0 - $g_t * $g_t);
  $df_t = $dc * $c_prev;
  $da_f = $df_t * $f_t * (1.0 - $f_t);
  $dw_i = $dw_i + $da_i * $seq[$t];
  $du_i = $du_i + $da_i * $h_prev;
  $db_i = $db_i + $da_i;
  $dw_f = $dw_f + $da_f * $seq[$t];
  $du_f = $du_f + $da_f * $h_prev;
  $db_f = $db_f + $da_f;
  $dw_o = $dw_o + $da_o * $seq[$t];
  $du_o = $du_o + $da_o * $h_prev;
  $db_o = $db_o + $da_o;
  $dw_c = $dw_c + $da_g * $seq[$t];
  $du_c = $du_c + $da_g * $h_prev;
  $db_c = $db_c + $da_g;
  $dh_next = $da_i * $w['u_i'] + $da_f * $w['u_f'] + $da_o * $w['u_o'] + $da_g * $w['u_c'];
  $dc_next = $dc * $f_t;
  $t = $t - 1;
};
  $w['w_y'] = $w['w_y'] - $lr * $dw_y;
  $w['b_y'] = $w['b_y'] - $lr * $db_y;
  $w['w_i'] = $w['w_i'] - $lr * $dw_i;
  $w['u_i'] = $w['u_i'] - $lr * $du_i;
  $w['b_i'] = $w['b_i'] - $lr * $db_i;
  $w['w_f'] = $w['w_f'] - $lr * $dw_f;
  $w['u_f'] = $w['u_f'] - $lr * $du_f;
  $w['b_f'] = $w['b_f'] - $lr * $db_f;
  $w['w_o'] = $w['w_o'] - $lr * $dw_o;
  $w['u_o'] = $w['u_o'] - $lr * $du_o;
  $w['b_o'] = $w['b_o'] - $lr * $db_o;
  $w['w_c'] = $w['w_c'] - $lr * $dw_c;
  $w['u_c'] = $w['u_c'] - $lr * $du_c;
  $w['b_c'] = $w['b_c'] - $lr * $db_c;
  return $w;
};
  function make_samples($data, $look_back) {
  global $epochs, $lr, $w, $test_seq, $pred;
  $X = [];
  $Y = [];
  $i = 0;
  while ($i + $look_back < count($data)) {
  $seq = array_slice($data, $i, $i + $look_back - $i);
  $X = _append($X, $seq);
  $Y = _append($Y, $data[$i + $look_back]);
  $i = $i + 1;
};
  return ['x' => $X, 'y' => $Y];
};
  function init_weights() {
  global $data, $look_back, $epochs, $lr, $w, $test_seq, $pred;
  return ['w_i' => 0.1, 'u_i' => 0.2, 'b_i' => 0.0, 'w_f' => 0.1, 'u_f' => 0.2, 'b_f' => 0.0, 'w_o' => 0.1, 'u_o' => 0.2, 'b_o' => 0.0, 'w_c' => 0.1, 'u_c' => 0.2, 'b_c' => 0.0, 'w_y' => 0.1, 'b_y' => 0.0];
};
  function train($data, $look_back, $epochs, $lr) {
  global $test_seq, $pred;
  $samples = make_samples($data, $look_back);
  $w = init_weights();
  $ep = 0;
  while ($ep < $epochs) {
  $j = 0;
  while ($j < _len($samples['x'])) {
  $seq = $samples['x'][$j];
  $target = $samples['y'][$j];
  $state = forward($seq, $w);
  $w = backward($seq, $target, $w, $state, $lr);
  $j = $j + 1;
};
  $ep = $ep + 1;
};
  return $w;
};
  function predict($seq, $w) {
  global $data, $look_back, $epochs, $lr, $test_seq, $pred;
  $state = forward($seq, $w);
  $h_last = $state['h'][_len($state['h']) - 1];
  return $w['w_y'] * $h_last + $w['b_y'];
};
  $data = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8];
  $look_back = 3;
  $epochs = 200;
  $lr = 0.1;
  $w = train($data, $look_back, $epochs, $lr);
  $test_seq = [0.6, 0.7, 0.8];
  $pred = predict($test_seq, $w);
  echo rtrim('Predicted value: ' . _str($pred)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
