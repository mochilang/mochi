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
$valid_colors = ['Black', 'Brown', 'Red', 'Orange', 'Yellow', 'Green', 'Blue', 'Violet', 'Grey', 'White', 'Gold', 'Silver'];
$significant_figures_color_values = ['Black' => 0, 'Brown' => 1, 'Red' => 2, 'Orange' => 3, 'Yellow' => 4, 'Green' => 5, 'Blue' => 6, 'Violet' => 7, 'Grey' => 8, 'White' => 9];
$multiplier_color_values = ['Black' => 1.0, 'Brown' => 10.0, 'Red' => 100.0, 'Orange' => 1000.0, 'Yellow' => 10000.0, 'Green' => 100000.0, 'Blue' => 1000000.0, 'Violet' => 10000000.0, 'Grey' => 100000000.0, 'White' => 1000000000.0, 'Gold' => 0.1, 'Silver' => 0.01];
$tolerance_color_values = ['Brown' => 1.0, 'Red' => 2.0, 'Orange' => 0.05, 'Yellow' => 0.02, 'Green' => 0.5, 'Blue' => 0.25, 'Violet' => 0.1, 'Grey' => 0.01, 'Gold' => 5.0, 'Silver' => 10.0];
$temperature_coeffecient_color_values = ['Black' => 250, 'Brown' => 100, 'Red' => 50, 'Orange' => 15, 'Yellow' => 25, 'Green' => 20, 'Blue' => 10, 'Violet' => 5, 'Grey' => 1];
function contains($list, $value) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  foreach ($list as $c) {
  if ($c == $value) {
  return true;
}
};
  return false;
}
function get_significant_digits($colors) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  $digit = 0;
  foreach ($colors as $color) {
  if (!(isset($significant_figures_color_values[$color]))) {
  $panic($color . ' is not a valid color for significant figure bands');
}
  $digit = $digit * 10 + $significant_figures_color_values[$color];
};
  return $digit;
}
function get_multiplier($color) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  if (!(isset($multiplier_color_values[$color]))) {
  $panic($color . ' is not a valid color for multiplier band');
}
  return $multiplier_color_values[$color];
}
function get_tolerance($color) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  if (!(isset($tolerance_color_values[$color]))) {
  $panic($color . ' is not a valid color for tolerance band');
}
  return $tolerance_color_values[$color];
}
function get_temperature_coeffecient($color) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  if (!(isset($temperature_coeffecient_color_values[$color]))) {
  $panic($color . ' is not a valid color for temperature coeffecient band');
}
  return $temperature_coeffecient_color_values[$color];
}
function get_band_type_count($total, $typ) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  if ($total == 3) {
  if ($typ == 'significant') {
  return 2;
};
  if ($typ == 'multiplier') {
  return 1;
};
  $panic($typ . ' is not valid for a 3 band resistor');
} else {
  if ($total == 4) {
  if ($typ == 'significant') {
  return 2;
};
  if ($typ == 'multiplier') {
  return 1;
};
  if ($typ == 'tolerance') {
  return 1;
};
  $panic($typ . ' is not valid for a 4 band resistor');
} else {
  if ($total == 5) {
  if ($typ == 'significant') {
  return 3;
};
  if ($typ == 'multiplier') {
  return 1;
};
  if ($typ == 'tolerance') {
  return 1;
};
  $panic($typ . ' is not valid for a 5 band resistor');
} else {
  if ($total == 6) {
  if ($typ == 'significant') {
  return 3;
};
  if ($typ == 'multiplier') {
  return 1;
};
  if ($typ == 'tolerance') {
  return 1;
};
  if ($typ == 'temp_coeffecient') {
  return 1;
};
  $panic($typ . ' is not valid for a 6 band resistor');
} else {
  $panic(_str($total) . ' is not a valid number of bands');
};
};
};
}
}
function check_validity($number_of_bands, $colors) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  if ($number_of_bands < 3 || $number_of_bands > 6) {
  $panic('Invalid number of bands. Resistor bands must be 3 to 6');
}
  if ($number_of_bands != count($colors)) {
  $panic('Expecting ' . _str($number_of_bands) . ' colors, provided ' . _str(count($colors)) . ' colors');
}
  foreach ($colors as $color) {
  if (!contains($valid_colors, $color)) {
  $panic($color . ' is not a valid color');
}
};
  return true;
}
function calculate_resistance($number_of_bands, $color_code_list) {
  global $valid_colors, $significant_figures_color_values, $multiplier_color_values, $tolerance_color_values, $temperature_coeffecient_color_values;
  check_validity($number_of_bands, $color_code_list);
  $sig_count = get_band_type_count($number_of_bands, 'significant');
  $significant_colors = array_slice($color_code_list, 0, $sig_count - 0);
  $significant_digits = get_significant_digits($significant_colors);
  $multiplier_color = $color_code_list[$sig_count];
  $multiplier = get_multiplier($multiplier_color);
  $tolerance = 20.0;
  if ($number_of_bands >= 4) {
  $tolerance_color = $color_code_list[$sig_count + 1];
  $tolerance = get_tolerance($tolerance_color);
}
  $temp_coeff = 0;
  if ($number_of_bands == 6) {
  $temp_color = $color_code_list[$sig_count + 2];
  $temp_coeff = get_temperature_coeffecient($temp_color);
}
  $resistance_value = $multiplier * $significant_digits;
  $resistance_str = _str($resistance_value);
  if ($resistance_value == intval($resistance_value)) {
  $resistance_str = _str(intval($resistance_value));
}
  $answer = $resistance_str . 'Ω ±' . _str($tolerance) . '% ';
  if ($temp_coeff != 0) {
  $answer = $answer . _str($temp_coeff) . ' ppm/K';
}
  return $answer;
}
