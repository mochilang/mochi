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
function absf($x) {
  global $rgb, $hsv;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function mochi_fmod($a, $b) {
  global $rgb, $hsv;
  return $a - $b * intval($a / $b);
}
function roundf($x) {
  global $rgb, $hsv;
  if ($x >= 0.0) {
  return intval($x + 0.5);
}
  return intval($x - 0.5);
}
function maxf($a, $b, $c) {
  global $rgb, $hsv;
  $m = $a;
  if ($b > $m) {
  $m = $b;
}
  if ($c > $m) {
  $m = $c;
}
  return $m;
}
function minf($a, $b, $c) {
  global $rgb, $hsv;
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
}
function hsv_to_rgb($hue, $saturation, $value) {
  global $rgb, $hsv;
  if ($hue < 0.0 || $hue > 360.0) {
  echo rtrim('hue should be between 0 and 360'), PHP_EOL;
  return [];
}
  if ($saturation < 0.0 || $saturation > 1.0) {
  echo rtrim('saturation should be between 0 and 1'), PHP_EOL;
  return [];
}
  if ($value < 0.0 || $value > 1.0) {
  echo rtrim('value should be between 0 and 1'), PHP_EOL;
  return [];
}
  $chroma = $value * $saturation;
  $hue_section = $hue / 60.0;
  $second_largest_component = $chroma * (1.0 - absf(mochi_fmod($hue_section, 2.0) - 1.0));
  $match_value = $value - $chroma;
  $red = 0;
  $green = 0;
  $blue = 0;
  if ($hue_section >= 0.0 && $hue_section <= 1.0) {
  $red = roundf(255.0 * ($chroma + $match_value));
  $green = roundf(255.0 * ($second_largest_component + $match_value));
  $blue = roundf(255.0 * $match_value);
} else {
  if ($hue_section > 1.0 && $hue_section <= 2.0) {
  $red = roundf(255.0 * ($second_largest_component + $match_value));
  $green = roundf(255.0 * ($chroma + $match_value));
  $blue = roundf(255.0 * $match_value);
} else {
  if ($hue_section > 2.0 && $hue_section <= 3.0) {
  $red = roundf(255.0 * $match_value);
  $green = roundf(255.0 * ($chroma + $match_value));
  $blue = roundf(255.0 * ($second_largest_component + $match_value));
} else {
  if ($hue_section > 3.0 && $hue_section <= 4.0) {
  $red = roundf(255.0 * $match_value);
  $green = roundf(255.0 * ($second_largest_component + $match_value));
  $blue = roundf(255.0 * ($chroma + $match_value));
} else {
  if ($hue_section > 4.0 && $hue_section <= 5.0) {
  $red = roundf(255.0 * ($second_largest_component + $match_value));
  $green = roundf(255.0 * $match_value);
  $blue = roundf(255.0 * ($chroma + $match_value));
} else {
  $red = roundf(255.0 * ($chroma + $match_value));
  $green = roundf(255.0 * $match_value);
  $blue = roundf(255.0 * ($second_largest_component + $match_value));
};
};
};
};
}
  return [$red, $green, $blue];
}
function rgb_to_hsv($red, $green, $blue) {
  global $rgb, $hsv;
  if ($red < 0 || $red > 255) {
  echo rtrim('red should be between 0 and 255'), PHP_EOL;
  return [];
}
  if ($green < 0 || $green > 255) {
  echo rtrim('green should be between 0 and 255'), PHP_EOL;
  return [];
}
  if ($blue < 0 || $blue > 255) {
  echo rtrim('blue should be between 0 and 255'), PHP_EOL;
  return [];
}
  $float_red = $red / 255.0;
  $float_green = $green / 255.0;
  $float_blue = $blue / 255.0;
  $value = maxf($float_red, $float_green, $float_blue);
  $min_val = minf($float_red, $float_green, $float_blue);
  $chroma = $value - $min_val;
  $saturation = ($value == 0.0 ? 0.0 : $chroma / $value);
  $hue = null;
  if ($chroma == 0.0) {
  $hue = 0.0;
} else {
  if ($value == $float_red) {
  $hue = 60.0 * (0.0 + ($float_green - $float_blue) / $chroma);
} else {
  if ($value == $float_green) {
  $hue = 60.0 * (2.0 + ($float_blue - $float_red) / $chroma);
} else {
  $hue = 60.0 * (4.0 + ($float_red - $float_green) / $chroma);
};
};
}
  $hue = mochi_fmod($hue + 360.0, 360.0);
  return [$hue, $saturation, $value];
}
function approximately_equal_hsv($hsv1, $hsv2) {
  global $rgb, $hsv;
  $check_hue = absf($hsv1[0] - $hsv2[0]) < 0.2;
  $check_saturation = absf($hsv1[1] - $hsv2[1]) < 0.002;
  $check_value = absf($hsv1[2] - $hsv2[2]) < 0.002;
  return $check_hue && $check_saturation && $check_value;
}
$rgb = hsv_to_rgb(180.0, 0.5, 0.5);
echo rtrim(_str($rgb)), PHP_EOL;
$hsv = rgb_to_hsv(64, 128, 128);
echo rtrim(_str($hsv)), PHP_EOL;
echo rtrim(_str(approximately_equal_hsv($hsv, [180.0, 0.5, 0.5]))), PHP_EOL;
