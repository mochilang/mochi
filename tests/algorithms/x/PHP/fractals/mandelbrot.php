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
function round_int($x) {
  global $img1, $img2;
  return intval(($x + 0.5));
}
function hsv_to_rgb($h, $s, $v) {
  global $img1, $img2;
  $i = intval(($h * 6.0));
  $f = $h * 6.0 - (floatval($i));
  $p = $v * (1.0 - $s);
  $q = $v * (1.0 - $f * $s);
  $t = $v * (1.0 - (1.0 - $f) * $s);
  $mod = $i % 6;
  $r = 0.0;
  $g = 0.0;
  $b = 0.0;
  if ($mod == 0) {
  $r = $v;
  $g = $t;
  $b = $p;
} else {
  if ($mod == 1) {
  $r = $q;
  $g = $v;
  $b = $p;
} else {
  if ($mod == 2) {
  $r = $p;
  $g = $v;
  $b = $t;
} else {
  if ($mod == 3) {
  $r = $p;
  $g = $q;
  $b = $v;
} else {
  if ($mod == 4) {
  $r = $t;
  $g = $p;
  $b = $v;
} else {
  $r = $v;
  $g = $p;
  $b = $q;
};
};
};
};
}
  return ['r' => round_int($r * 255.0), 'g' => round_int($g * 255.0), 'b' => round_int($b * 255.0)];
}
function get_distance($x, $y, $max_step) {
  global $img1, $img2;
  $a = $x;
  $b = $y;
  $step = -1;
  while ($step < $max_step - 1) {
  $step = $step + 1;
  $a_new = $a * $a - $b * $b + $x;
  $b = 2.0 * $a * $b + $y;
  $a = $a_new;
  if ($a * $a + $b * $b > 4.0) {
  break;
}
};
  return (floatval($step)) / (floatval(($max_step - 1)));
}
function get_black_and_white_rgb($distance) {
  global $img1, $img2;
  if ($distance == 1.0) {
  return ['r' => 0, 'g' => 0, 'b' => 0];
} else {
  return ['r' => 255, 'g' => 255, 'b' => 255];
}
}
function get_color_coded_rgb($distance) {
  global $img1, $img2;
  if ($distance == 1.0) {
  return ['r' => 0, 'g' => 0, 'b' => 0];
} else {
  return hsv_to_rgb($distance, 1.0, 1.0);
}
}
function get_image($image_width, $image_height, $figure_center_x, $figure_center_y, $figure_width, $max_step, $use_distance_color_coding) {
  global $img1, $img2;
  $img = [];
  $figure_height = $figure_width / (floatval($image_width)) * (floatval($image_height));
  $image_y = 0;
  while ($image_y < $image_height) {
  $row = [];
  $image_x = 0;
  while ($image_x < $image_width) {
  $fx = $figure_center_x + ((floatval($image_x)) / (floatval($image_width)) - 0.5) * $figure_width;
  $fy = $figure_center_y + ((floatval($image_y)) / (floatval($image_height)) - 0.5) * $figure_height;
  $distance = get_distance($fx, $fy, $max_step);
  $rgb = null;
  if ($use_distance_color_coding) {
  $rgb = get_color_coded_rgb($distance);
} else {
  $rgb = get_black_and_white_rgb($distance);
}
  $row = _append($row, $rgb);
  $image_x = $image_x + 1;
};
  $img = _append($img, $row);
  $image_y = $image_y + 1;
};
  return $img;
}
function rgb_to_string($c) {
  global $img1, $img2;
  return '(' . _str($c['r']) . ', ' . _str($c['g']) . ', ' . _str($c['b']) . ')';
}
$img1 = get_image(10, 10, -0.6, 0.0, 3.2, 50, true);
echo rtrim(rgb_to_string($img1[0][0])), PHP_EOL;
$img2 = get_image(10, 10, -0.6, 0.0, 3.2, 50, false);
echo rtrim(rgb_to_string($img2[0][0])), PHP_EOL;
