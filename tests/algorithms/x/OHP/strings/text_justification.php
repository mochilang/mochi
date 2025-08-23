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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function repeat_str($s, $count) {
  $res = '';
  $i = 0;
  while ($i < $count) {
  $res = $res . $s;
  $i = $i + 1;
};
  return $res;
};
  function split_words($s) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if ($current != '') {
  $res = _append($res, $current);
  $current = '';
};
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  if ($current != '') {
  $res = _append($res, $current);
}
  return $res;
};
  function justify_line($line, $width, $max_width) {
  $overall_spaces_count = $max_width - $width;
  $words_count = count($line);
  if ($words_count == 1) {
  return $line[0] . repeat_str(' ', $overall_spaces_count);
}
  $spaces_to_insert_between_words = $words_count - 1;
  $num_spaces_between_words_list = [];
  $base = _intdiv($overall_spaces_count, $spaces_to_insert_between_words);
  $extra = $overall_spaces_count % $spaces_to_insert_between_words;
  $i = 0;
  while ($i < $spaces_to_insert_between_words) {
  $spaces = $base;
  if ($i < $extra) {
  $spaces = $spaces + 1;
}
  $num_spaces_between_words_list = _append($num_spaces_between_words_list, $spaces);
  $i = $i + 1;
};
  $aligned = '';
  $i = 0;
  while ($i < $spaces_to_insert_between_words) {
  $aligned = $aligned . $line[$i] . repeat_str(' ', $num_spaces_between_words_list[$i]);
  $i = $i + 1;
};
  $aligned = $aligned . $line[$spaces_to_insert_between_words];
  return $aligned;
};
  function text_justification($word, $max_width) {
  $words = split_words($word);
  $answer = [];
  $line = [];
  $width = 0;
  $idx = 0;
  while ($idx < count($words)) {
  $w = $words[$idx];
  if ($width + strlen($w) + count($line) <= $max_width) {
  $line = _append($line, $w);
  $width = $width + strlen($w);
} else {
  $answer = _append($answer, justify_line($line, $width, $max_width));
  $line = [$w];
  $width = strlen($w);
}
  $idx = $idx + 1;
};
  $remaining_spaces = $max_width - $width - count($line);
  $last_line = '';
  $j = 0;
  while ($j < count($line)) {
  if ($j > 0) {
  $last_line = $last_line . ' ';
}
  $last_line = $last_line . $line[$j];
  $j = $j + 1;
};
  $last_line = $last_line . repeat_str(' ', $remaining_spaces + 1);
  $answer = _append($answer, $last_line);
  return $answer;
};
  echo rtrim(_str(text_justification('This is an example of text justification.', 16))), PHP_EOL;
  echo rtrim(_str(text_justification('Two roads diverged in a yellow wood', 16))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
