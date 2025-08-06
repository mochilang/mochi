<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$K = [0.33, 0.44, 0.55, 0.44, 0.33];
$t = 3;
$size = 5;
function round_dec($x, $n) {
  global $K, $t, $size, $machine, $res;
  $m10 = 1.0;
  $i = 0;
  while ($i < $n) {
  $m10 = $m10 * 10.0;
  $i = $i + 1;
};
  $y = $x * $m10 + 0.5;
  return (1.0 * intval($y)) / $m10;
}
function reset() {
  global $K, $t, $size, $machine, $i, $res;
  return ['buffer' => $K, 'params' => [0.0, 0.0, 0.0, 0.0, 0.0], 'time' => 0];
}
function push($m, $seed) {
  global $K, $t, $size, $machine, $res;
  $buf = $m['buffer'];
  $par = $m['params'];
  $i = 0;
  while ($i < count($buf)) {
  $value = $buf[$i];
  $e = (1.0 * $seed) / $value;
  $next_value = $buf[($i + 1) % $size] + $e;
  $next_value = $next_value - (1.0 * intval($next_value));
  $r = $par[$i] + $e;
  $r = $r - (1.0 * intval($r));
  $r = $r + 3.0;
  $buf[$i] = round_dec($r * $next_value * (1.0 - $next_value), 10);
  $par[$i] = $r;
  $i = $i + 1;
};
  return ['buffer' => $buf, 'params' => $par, 'time' => $m['time'] + 1];
}
function mochi_xor($a, $b) {
  global $K, $t, $size, $machine, $i;
  $aa = $a;
  $bb = $b;
  $res = 0;
  $bit = 1;
  while ($aa > 0 || $bb > 0) {
  $abit = $aa % 2;
  $bbit = $bb % 2;
  if ($abit != $bbit) {
  $res = $res + $bit;
}
  $aa = _intdiv($aa, 2);
  $bb = _intdiv($bb, 2);
  $bit = $bit * 2;
};
  return $res;
}
function xorshift($x, $y) {
  global $K, $t, $size, $machine, $i, $res;
  $xv = $x;
  $yv = $y;
  $xv = mochi_xor($xv, _intdiv($yv, 8192));
  $yv = mochi_xor($yv, $xv * 131072);
  $xv = mochi_xor($xv, _intdiv($yv, 32));
  return $xv;
}
function pull($m) {
  global $K, $t, $size, $machine, $res;
  $buf = $m['buffer'];
  $par = $m['params'];
  $key = fmod($m['time'], $size);
  $i = 0;
  while ($i < $t) {
  $r = $par[$key];
  $value = $buf[$key];
  $buf[$key] = round_dec($r * $value * (1.0 - $value), 10);
  $new_r = (1.0 * $m['time']) * 0.01 + $r * 1.01;
  $new_r = $new_r - (1.0 * intval($new_r));
  $par[$key] = $new_r + 3.0;
  $i = $i + 1;
};
  $x = intval($buf[($key + 2) % $size] * 10000000000.0);
  $y = intval($buf[($key + $size - 2) % $size] * 10000000000.0);
  $new_machine = ['buffer' => $buf, 'params' => $par, 'time' => $m['time'] + 1];
  $value = fmod(xorshift($x, $y), 4294967295);
  return ['value' => $value, 'machine' => $new_machine];
}
$machine = reset();
$i = 0;
while ($i < 100) {
  $machine = push($machine, $i);
  $i = $i + 1;
}
$res = pull($machine);
echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
echo rtrim(json_encode($res['machine']['buffer'], 1344)), PHP_EOL;
echo rtrim(json_encode($res['machine']['params'], 1344)), PHP_EOL;
