<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function make_matrix_bool($rows, $cols, $init) {
  $matrix = [];
  for ($_ = 0; $_ < $rows; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $cols; $_2++) {
  $row = _append($row, $init);
};
  $matrix = _append($matrix, $row);
};
  return $matrix;
};
  function match_pattern($input_string, $pattern) {
  $len_string = strlen($input_string) + 1;
  $len_pattern = strlen($pattern) + 1;
  $dp = make_matrix_bool($len_string, $len_pattern, false);
  $row0 = $dp[0];
  $row0[0] = true;
  $dp[0] = $row0;
  $j = 1;
  while ($j < $len_pattern) {
  $row0 = $dp[0];
  if (substr($pattern, $j - 1, $j - ($j - 1)) == '*') {
  $row0[$j] = $row0[$j - 2];
} else {
  $row0[$j] = false;
}
  $dp[0] = $row0;
  $j = $j + 1;
};
  $i = 1;
  while ($i < $len_string) {
  $row = $dp[$i];
  $j2 = 1;
  while ($j2 < $len_pattern) {
  $s_char = substr($input_string, $i - 1, $i - ($i - 1));
  $p_char = substr($pattern, $j2 - 1, $j2 - ($j2 - 1));
  if ($s_char == $p_char || $p_char == '.') {
  $row[$j2] = $dp[$i - 1][$j2 - 1];
} else {
  if ($p_char == '*') {
  $val = $dp[$i][$j2 - 2];
  $prev_p = substr($pattern, $j2 - 2, $j2 - 1 - ($j2 - 2));
  if (!$val && ($prev_p == $s_char || $prev_p == '.')) {
  $val = $dp[$i - 1][$j2];
};
  $row[$j2] = $val;
} else {
  $row[$j2] = false;
};
}
  $j2 = $j2 + 1;
};
  $dp[$i] = $row;
  $i = $i + 1;
};
  return $dp[$len_string - 1][$len_pattern - 1];
};
  function main() {
  if (!match_pattern('aab', 'c*a*b')) {
  _panic('case1 failed');
}
  if (match_pattern('dabc', '*abc')) {
  _panic('case2 failed');
}
  if (match_pattern('aaa', 'aa')) {
  _panic('case3 failed');
}
  if (!match_pattern('aaa', 'a.a')) {
  _panic('case4 failed');
}
  if (match_pattern('aaab', 'aa*')) {
  _panic('case5 failed');
}
  if (!match_pattern('aaab', '.*')) {
  _panic('case6 failed');
}
  if (match_pattern('a', 'bbbb')) {
  _panic('case7 failed');
}
  if (match_pattern('', 'bbbb')) {
  _panic('case8 failed');
}
  if (match_pattern('a', '')) {
  _panic('case9 failed');
}
  if (!match_pattern('', '')) {
  _panic('case10 failed');
}
  echo rtrim(_str(match_pattern('aab', 'c*a*b'))), PHP_EOL;
  echo rtrim(_str(match_pattern('dabc', '*abc'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaa', 'aa'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaa', 'a.a'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaab', 'aa*'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaab', '.*'))), PHP_EOL;
  echo rtrim(_str(match_pattern('a', 'bbbb'))), PHP_EOL;
  echo rtrim(_str(match_pattern('', 'bbbb'))), PHP_EOL;
  echo rtrim(_str(match_pattern('a', ''))), PHP_EOL;
  echo rtrim(_str(match_pattern('', ''))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
