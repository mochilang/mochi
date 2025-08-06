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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function round_int($x) {
  if ($x >= 0.0) {
  return intval($x + 0.5);
}
  return intval($x - 0.5);
};
  function zeros($rows, $cols) {
  $res = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function warp($image, $h_flow, $v_flow) {
  $h = count($image);
  $w = count($image[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $sx = $x - round_int($h_flow[$y][$x]);
  $sy = $y - round_int($v_flow[$y][$x]);
  if ($sx >= 0 && $sx < $w && $sy >= 0 && $sy < $h) {
  $row = _append($row, $image[$sy][$sx]);
} else {
  $row = _append($row, 0.0);
}
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
};
  function convolve($img, $ker) {
  $h = count($img);
  $w = count($img[0]);
  $kh = count($ker);
  $kw = count($ker[0]);
  $py = _intdiv($kh, 2);
  $px = _intdiv($kw, 2);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $s = 0.0;
  $ky = 0;
  while ($ky < $kh) {
  $kx = 0;
  while ($kx < $kw) {
  $iy = $y + $ky - $py;
  $ix = $x + $kx - $px;
  if ($iy >= 0 && $iy < $h && $ix >= 0 && $ix < $w) {
  $s = $s + $img[$iy][$ix] * $ker[$ky][$kx];
}
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  $row = _append($row, $s);
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
};
  function horn_schunck($image0, $image1, $num_iter, $alpha) {
  $h = count($image0);
  $w = count($image0[0]);
  $u = zeros($h, $w);
  $v = zeros($h, $w);
  $kernel_x = [[-0.25, 0.25], [-0.25, 0.25]];
  $kernel_y = [[-0.25, -0.25], [0.25, 0.25]];
  $kernel_t = [[0.25, 0.25], [0.25, 0.25]];
  $laplacian = [[0.0833333333333, 0.166666666667, 0.0833333333333], [0.166666666667, 0.0, 0.166666666667], [0.0833333333333, 0.166666666667, 0.0833333333333]];
  $it = 0;
  while ($it < $num_iter) {
  $warped = warp($image0, $u, $v);
  $dx1 = convolve($warped, $kernel_x);
  $dx2 = convolve($image1, $kernel_x);
  $dy1 = convolve($warped, $kernel_y);
  $dy2 = convolve($image1, $kernel_y);
  $dt1 = convolve($warped, $kernel_t);
  $dt2 = convolve($image1, $kernel_t);
  $avg_u = convolve($u, $laplacian);
  $avg_v = convolve($v, $laplacian);
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $dx = $dx1[$y][$x] + $dx2[$y][$x];
  $dy = $dy1[$y][$x] + $dy2[$y][$x];
  $dt = $dt1[$y][$x] - $dt2[$y][$x];
  $au = $avg_u[$y][$x];
  $av = $avg_v[$y][$x];
  $numer = $dx * $au + $dy * $av + $dt;
  $denom = $alpha * $alpha + $dx * $dx + $dy * $dy;
  $upd = $numer / $denom;
  $u[$y][$x] = $au - $dx * $upd;
  $v[$y][$x] = $av - $dy * $upd;
  $x = $x + 1;
};
  $y = $y + 1;
};
  $it = $it + 1;
};
  return [$u, $v];
};
  function print_matrix($mat) {
  $y = 0;
  while ($y < count($mat)) {
  $row = $mat[$y];
  $x = 0;
  $line = '';
  while ($x < count($row)) {
  $line = $line . _str(round_int($row[$x]));
  if ($x + 1 < count($row)) {
  $line = $line . ' ';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
};
  function main() {
  $image0 = [[0.0, 0.0, 2.0], [0.0, 0.0, 2.0]];
  $image1 = [[0.0, 2.0, 0.0], [0.0, 2.0, 0.0]];
  $flows = horn_schunck($image0, $image1, 20, 0.1);
  $u = $flows[0];
  $v = $flows[1];
  print_matrix($u);
  echo rtrim('---'), PHP_EOL;
  print_matrix($v);
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
