<?php
ini_set('memory_limit', '-1');
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
function sqrtApprox($x) {
  global $dxs, $dys;
  if ($x <= 0.0) {
  return 0.0;
}
  $g = $x;
  $i = 0;
  while ($i < 20) {
  $g = ($g + $x / $g) / 2.0;
  $i = $i + 1;
};
  return $g;
}
$dxs = [-0.533, 0.27, 0.859, -0.043, -0.205, -0.127, -0.071, 0.275, 1.251, -0.231, -0.401, 0.269, 0.491, 0.951, 1.15, 0.001, -0.382, 0.161, 0.915, 2.08, -2.337, 0.034, -0.126, 0.014, 0.709, 0.129, -1.093, -0.483, -1.193, 0.02, -0.051, 0.047, -0.095, 0.695, 0.34, -0.182, 0.287, 0.213, -0.423, -0.021, -0.134, 1.798, 0.021, -1.099, -0.361, 1.636, -1.134, 1.315, 0.201, 0.034, 0.097, -0.17, 0.054, -0.553, -0.024, -0.181, -0.7, -0.361, -0.789, 0.279, -0.174, -0.009, -0.323, -0.658, 0.348, -0.528, 0.881, 0.021, -0.853, 0.157, 0.648, 1.774, -1.043, 0.051, 0.021, 0.247, -0.31, 0.171, 0.0, 0.106, 0.024, -0.386, 0.962, 0.765, -0.125, -0.289, 0.521, 0.017, 0.281, -0.749, -0.149, -2.436, -0.909, 0.394, -0.113, -0.598, 0.443, -0.521, -0.799, 0.087];
$dys = [0.136, 0.717, 0.459, -0.225, 1.392, 0.385, 0.121, -0.395, 0.49, -0.682, -0.065, 0.242, -0.288, 0.658, 0.459, 0.0, 0.426, 0.205, -0.765, -2.188, -0.742, -0.01, 0.089, 0.208, 0.585, 0.633, -0.444, -0.351, -1.087, 0.199, 0.701, 0.096, -0.025, -0.868, 1.051, 0.157, 0.216, 0.162, 0.249, -0.007, 0.009, 0.508, -0.79, 0.723, 0.881, -0.508, 0.393, -0.226, 0.71, 0.038, -0.217, 0.831, 0.48, 0.407, 0.447, -0.295, 1.126, 0.38, 0.549, -0.445, -0.046, 0.428, -0.074, 0.217, -0.822, 0.491, 1.347, -0.141, 1.23, -0.044, 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, -0.729, 0.65, -1.103, 0.154, -1.72, 0.051, -0.385, 0.477, 1.537, -0.901, 0.939, -0.411, 0.341, -0.411, 0.106, 0.224, -0.947, -1.424, -0.542, -1.032];
function funnel($fa, $r) {
  global $dxs, $dys;
  $x = 0.0;
  $result = [];
  $i = 0;
  while ($i < count($fa)) {
  $f = $fa[$i];
  $result = array_merge($result, [$x + $f]);
  $x = $r($x, $f);
  $i = $i + 1;
};
  return $result;
}
function mean($fa) {
  global $dxs, $dys;
  $sum = 0.0;
  $i = 0;
  while ($i < count($fa)) {
  $sum = $sum + $fa[$i];
  $i = $i + 1;
};
  return $sum / (floatval(count($fa)));
}
function stdDev($fa) {
  global $dxs, $dys;
  $m = mean($fa);
  $sum = 0.0;
  $i = 0;
  while ($i < count($fa)) {
  $d = $fa[$i] - $m;
  $sum = $sum + $d * $d;
  $i = $i + 1;
};
  $r = sqrtApprox($sum / (floatval(count($fa))));
  return $r;
}
function experiment($label, $r) {
  global $dxs, $dys;
  $rxs = funnel($dxs, $r);
  $rys = funnel($dys, $r);
  echo rtrim($label . '  :      x        y'), PHP_EOL;
  echo rtrim('Mean    :  ' . _str(mean($rxs)) . ', ' . _str(mean($rys))), PHP_EOL;
  echo rtrim('Std Dev :  ' . _str(stdDev($rxs)) . ', ' . _str(stdDev($rys))), PHP_EOL;
  echo rtrim(''), PHP_EOL;
}
function main() {
  global $dxs, $dys;
  experiment('Rule 1', function($x, $y) {
  return 0.0;
});
  experiment('Rule 2', function($x, $dz) {
  return -$dz;
});
  experiment('Rule 3', function($z, $dz) {
  return -($z + $dz);
});
  experiment('Rule 4', function($z, $dz) {
  return $z + $dz;
});
}
main();
