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
function compare_string($string1, $string2) {
  $result = '';
  $count = 0;
  $i = 0;
  while ($i < strlen($string1)) {
  $c1 = substr($string1, $i, $i + 1 - $i);
  $c2 = substr($string2, $i, $i + 1 - $i);
  if ($c1 != $c2) {
  $count = $count + 1;
  $result = $result . '_';
} else {
  $result = $result . $c1;
}
  $i = $i + 1;
};
  if ($count > 1) {
  return '';
}
  return $result;
}
function contains_string($arr, $value) {
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function unique_strings($arr) {
  $res = [];
  $i = 0;
  while ($i < count($arr)) {
  if (!contains_string($res, $arr[$i])) {
  $res = _append($res, $arr[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function check($binary) {
  $pi = [];
  $current = $binary;
  while (true) {
  $check1 = [];
  $i = 0;
  while ($i < count($current)) {
  $check1 = _append($check1, '$');
  $i = $i + 1;
};
  $temp = [];
  $i = 0;
  while ($i < count($current)) {
  $j = $i + 1;
  while ($j < count($current)) {
  $k = compare_string($current[$i], $current[$j]);
  if ($k == '') {
  $check1[$i] = '*';
  $check1[$j] = '*';
  $temp = _append($temp, 'X');
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($current)) {
  if ($check1[$i] == '$') {
  $pi = _append($pi, $current[$i]);
}
  $i = $i + 1;
};
  if (count($temp) == 0) {
  return $pi;
}
  $current = unique_strings($temp);
};
}
function decimal_to_binary($no_of_variable, $minterms) {
  $temp = [];
  $idx = 0;
  while ($idx < count($minterms)) {
  $minterm = $minterms[$idx];
  $string = '';
  $i = 0;
  while ($i < $no_of_variable) {
  $string = _str($minterm % 2) . $string;
  $minterm = _intdiv($minterm, 2);
  $i = $i + 1;
};
  $temp = _append($temp, $string);
  $idx = $idx + 1;
};
  return $temp;
}
function is_for_table($string1, $string2, $count) {
  $count_n = 0;
  $i = 0;
  while ($i < strlen($string1)) {
  $c1 = substr($string1, $i, $i + 1 - $i);
  $c2 = substr($string2, $i, $i + 1 - $i);
  if ($c1 != $c2) {
  $count_n = $count_n + 1;
}
  $i = $i + 1;
};
  return $count_n == $count;
}
function count_ones($row) {
  $c = 0;
  $j = 0;
  while ($j < count($row)) {
  if ($row[$j] == 1) {
  $c = $c + 1;
}
  $j = $j + 1;
};
  return $c;
}
function selection(&$chart, $prime_implicants) {
  $temp = [];
  $select = [];
  $i = 0;
  while ($i < count($chart)) {
  $select = _append($select, 0);
  $i = $i + 1;
};
  $col = 0;
  while ($col < count($chart[0])) {
  $count = 0;
  $row = 0;
  while ($row < count($chart)) {
  if ($chart[$row][$col] == 1) {
  $count = $count + 1;
}
  $row = $row + 1;
};
  if ($count == 1) {
  $rem = 0;
  $row = 0;
  while ($row < count($chart)) {
  if ($chart[$row][$col] == 1) {
  $rem = $row;
}
  $row = $row + 1;
};
  $select[$rem] = 1;
}
  $col = $col + 1;
};
  $i = 0;
  while ($i < count($select)) {
  if ($select[$i] == 1) {
  $j = 0;
  while ($j < count($chart[0])) {
  if ($chart[$i][$j] == 1) {
  $r = 0;
  while ($r < count($chart)) {
  $chart[$r][$j] = 0;
  $r = $r + 1;
};
}
  $j = $j + 1;
};
  $temp = _append($temp, $prime_implicants[$i]);
}
  $i = $i + 1;
};
  while (true) {
  $counts = [];
  $r = 0;
  while ($r < count($chart)) {
  $counts = _append($counts, count_ones($chart[$r]));
  $r = $r + 1;
};
  $max_n = $counts[0];
  $rem = 0;
  $k = 1;
  while ($k < count($counts)) {
  if ($counts[$k] > $max_n) {
  $max_n = $counts[$k];
  $rem = $k;
}
  $k = $k + 1;
};
  if ($max_n == 0) {
  return $temp;
}
  $temp = _append($temp, $prime_implicants[$rem]);
  $j = 0;
  while ($j < count($chart[0])) {
  if ($chart[$rem][$j] == 1) {
  $r2 = 0;
  while ($r2 < count($chart)) {
  $chart[$r2][$j] = 0;
  $r2 = $r2 + 1;
};
}
  $j = $j + 1;
};
};
}
function count_char($s, $ch) {
  $cnt = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
}
function prime_implicant_chart($prime_implicants, $binary) {
  $chart = [];
  $i = 0;
  while ($i < count($prime_implicants)) {
  $row = [];
  $j = 0;
  while ($j < count($binary)) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $chart = _append($chart, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($prime_implicants)) {
  $count = count_char($prime_implicants[$i], '_');
  $j = 0;
  while ($j < count($binary)) {
  if (is_for_table($prime_implicants[$i], $binary[$j], $count)) {
  $chart[$i][$j] = 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $chart;
}
function main() {
  $no_of_variable = 3;
  $minterms = [1, 5, 7];
  $binary = decimal_to_binary($no_of_variable, $minterms);
  $prime_implicants = check($binary);
  echo rtrim('Prime Implicants are:'), PHP_EOL;
  echo rtrim(_str($prime_implicants)), PHP_EOL;
  $chart = prime_implicant_chart($prime_implicants, $binary);
  $essential_prime_implicants = selection($chart, $prime_implicants);
  echo rtrim('Essential Prime Implicants are:'), PHP_EOL;
  echo rtrim(_str($essential_prime_implicants)), PHP_EOL;
}
main();
