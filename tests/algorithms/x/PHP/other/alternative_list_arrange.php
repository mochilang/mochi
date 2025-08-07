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
$__start_mem = memory_get_usage();
$__start = _now();
  function from_int($x) {
  global $example1, $example2, $example3, $example4;
  return ['__tag' => 'Int', 'value' => $x];
};
  function from_string($s) {
  global $example1, $example2, $example3, $example4;
  return ['__tag' => 'Str', 'value' => $s];
};
  function item_to_string($it) {
  global $example1, $example2, $example3, $example4;
  return match($it) {
    $Int($v) => _str($v),
    $Str($s) => $s,
};
};
  function alternative_list_arrange($first, $second) {
  global $example1, $example2, $example3, $example4;
  $len1 = count($first);
  $len2 = count($second);
  $abs_len = ($len1 > $len2 ? $len1 : $len2);
  $result = [];
  $i = 0;
  while ($i < $abs_len) {
  if ($i < $len1) {
  $result = _append($result, $first[$i]);
}
  if ($i < $len2) {
  $result = _append($result, $second[$i]);
}
  $i = $i + 1;
};
  return $result;
};
  function list_to_string($xs) {
  global $example1, $example2, $example3, $example4;
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . item_to_string($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  $example1 = alternative_list_arrange([from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)], [from_string('A'), from_string('B'), from_string('C')]);
  echo rtrim(list_to_string($example1)), PHP_EOL;
  $example2 = alternative_list_arrange([from_string('A'), from_string('B'), from_string('C')], [from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)]);
  echo rtrim(list_to_string($example2)), PHP_EOL;
  $example3 = alternative_list_arrange([from_string('X'), from_string('Y'), from_string('Z')], [from_int(9), from_int(8), from_int(7), from_int(6)]);
  echo rtrim(list_to_string($example3)), PHP_EOL;
  $example4 = alternative_list_arrange([from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)], []);
  echo rtrim(list_to_string($example4)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
