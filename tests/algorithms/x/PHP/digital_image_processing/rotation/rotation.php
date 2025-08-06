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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function mat_inverse3($m) {
  global $img, $pts1, $pts2, $rotated;
  $a = $m[0][0];
  $b = $m[0][1];
  $c = $m[0][2];
  $d = $m[1][0];
  $e = $m[1][1];
  $f = $m[1][2];
  $g = $m[2][0];
  $h = $m[2][1];
  $i = $m[2][2];
  $det = $a * ($e * $i - $f * $h) - $b * ($d * $i - $f * $g) + $c * ($d * $h - $e * $g);
  if ($det == 0.0) {
  $panic('singular matrix');
}
  $adj00 = $e * $i - $f * $h;
  $adj01 = $c * $h - $b * $i;
  $adj02 = $b * $f - $c * $e;
  $adj10 = $f * $g - $d * $i;
  $adj11 = $a * $i - $c * $g;
  $adj12 = $c * $d - $a * $f;
  $adj20 = $d * $h - $e * $g;
  $adj21 = $b * $g - $a * $h;
  $adj22 = $a * $e - $b * $d;
  $inv = [];
  $inv = _append($inv, [$adj00 / $det, $adj01 / $det, $adj02 / $det]);
  $inv = _append($inv, [$adj10 / $det, $adj11 / $det, $adj12 / $det]);
  $inv = _append($inv, [$adj20 / $det, $adj21 / $det, $adj22 / $det]);
  return $inv;
}
function mat_vec_mul($m, $v) {
  global $img, $pts1, $pts2, $rotated;
  $res = [];
  $i = 0;
  while ($i < 3) {
  $val = $m[$i][0] * $v[0] + $m[$i][1] * $v[1] + $m[$i][2] * $v[2];
  $res = _append($res, $val);
  $i = $i + 1;
};
  return $res;
}
function create_matrix($rows, $cols, $value) {
  global $img, $pts1, $pts2, $rotated;
  $result = [];
  $r = 0;
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $cols) {
  $row = _append($row, $value);
  $c = $c + 1;
};
  $result = _append($result, $row);
  $r = $r + 1;
};
  return $result;
}
function round_to_int($x) {
  global $img, $pts1, $pts2, $rotated;
  if ($x >= 0.0) {
  return intval($x + 0.5);
}
  return intval($x - 0.5);
}
function get_rotation($img, $pt1, $pt2, $rows, $cols) {
  global $pts1, $pts2, $rotated;
  $src = [[$pt1[0][0], $pt1[0][1], 1.0], [$pt1[1][0], $pt1[1][1], 1.0], [$pt1[2][0], $pt1[2][1], 1.0]];
  $inv = mat_inverse3($src);
  $vecx = [$pt2[0][0], $pt2[1][0], $pt2[2][0]];
  $vecy = [$pt2[0][1], $pt2[1][1], $pt2[2][1]];
  $avec = mat_vec_mul($inv, $vecx);
  $bvec = mat_vec_mul($inv, $vecy);
  $a0 = $avec[0];
  $a1 = $avec[1];
  $a2 = $avec[2];
  $b0 = $bvec[0];
  $b1 = $bvec[1];
  $b2 = $bvec[2];
  $out = create_matrix($rows, $cols, 0);
  $y = 0;
  while ($y < $rows) {
  $x = 0;
  while ($x < $cols) {
  $xf = $a0 * (1.0 * $x) + $a1 * (1.0 * $y) + $a2;
  $yf = $b0 * (1.0 * $x) + $b1 * (1.0 * $y) + $b2;
  $sx = round_to_int($xf);
  $sy = round_to_int($yf);
  if ($sx >= 0 && $sx < $cols && $sy >= 0 && $sy < $rows) {
  $out[$sy][$sx] = $img[$y][$x];
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $out;
}
$img = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
$pts1 = [[0.0, 0.0], [2.0, 0.0], [0.0, 2.0]];
$pts2 = [[0.0, 2.0], [0.0, 0.0], [2.0, 2.0]];
$rotated = get_rotation($img, $pts1, $pts2, 3, 3);
echo rtrim(_str($rotated)), PHP_EOL;
