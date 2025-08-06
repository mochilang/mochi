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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function rgb_to_gray($rgb) {
  $result = [];
  $i = 0;
  while ($i < count($rgb)) {
  $row = [];
  $j = 0;
  while ($j < count($rgb[$i])) {
  $r = $rgb[$i][$j][0];
  $g = $rgb[$i][$j][1];
  $b = $rgb[$i][$j][2];
  $gray = 0.2989 * (1.0 * $r) + 0.587 * (1.0 * $g) + 0.114 * (1.0 * $b);
  $row = _append($row, $gray);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function gray_to_binary($gray) {
  $result = [];
  $i = 0;
  while ($i < count($gray)) {
  $row = [];
  $j = 0;
  while ($j < count($gray[$i])) {
  $v = $gray[$i][$j];
  if ($v > 127.0 && $v <= 255.0) {
  $row = _append($row, 1);
} else {
  $row = _append($row, 0);
}
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function dilation($image, $kernel) {
  $img_h = count($image);
  $img_w = count($image[0]);
  $k_h = count($kernel);
  $k_w = count($kernel[0]);
  $pad_h = _intdiv($k_h, 2);
  $pad_w = _intdiv($k_w, 2);
  $p_h = $img_h + 2 * $pad_h;
  $p_w = $img_w + 2 * $pad_w;
  $padded = [];
  $i = 0;
  while ($i < $p_h) {
  $row = [];
  $j = 0;
  while ($j < $p_w) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $padded = _append($padded, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $img_h) {
  $j = 0;
  while ($j < $img_w) {
  $padded[$pad_h + $i][$pad_w + $j] = $image[$i][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  $output = [];
  $i = 0;
  while ($i < $img_h) {
  $row = [];
  $j = 0;
  while ($j < $img_w) {
  $sum = 0;
  $ky = 0;
  while ($ky < $k_h) {
  $kx = 0;
  while ($kx < $k_w) {
  if ($kernel[$ky][$kx] == 1) {
  $sum = $sum + $padded[$i + $ky][$j + $kx];
}
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  if ($sum > 0) {
  $row = _append($row, 1);
} else {
  $row = _append($row, 0);
}
  $j = $j + 1;
};
  $output = _append($output, $row);
  $i = $i + 1;
};
  return $output;
}
function print_float_matrix($mat) {
  $i = 0;
  while ($i < count($mat)) {
  $line = '';
  $j = 0;
  while ($j < count($mat[$i])) {
  $line = $line . _str($mat[$i][$j]);
  if ($j < count($mat[$i]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
function print_int_matrix($mat) {
  $i = 0;
  while ($i < count($mat)) {
  $line = '';
  $j = 0;
  while ($j < count($mat[$i])) {
  $line = $line . _str($mat[$i][$j]);
  if ($j < count($mat[$i]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
function main() {
  $rgb_example = [[[127, 255, 0]]];
  print_float_matrix(rgb_to_gray($rgb_example));
  $gray_example = [[26.0, 255.0, 14.0], [5.0, 147.0, 20.0], [1.0, 200.0, 0.0]];
  print_int_matrix(gray_to_binary($gray_example));
  $binary_image = [[0, 1, 0], [0, 1, 0], [0, 1, 0]];
  $kernel = [[0, 1, 0], [1, 1, 1], [0, 1, 0]];
  print_int_matrix(dilation($binary_image, $kernel));
}
main();
