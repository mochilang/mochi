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
$__start_mem = memory_get_usage();
$__start = _now();
  function monthUnique($b, $list) {
  global $choices, $filtered, $filtered2, $filtered3, $filtered4;
  $c = 0;
  foreach ($list as $x) {
  if ($x['month'] == $b['month']) {
  $c = $c + 1;
}
};
  return $c == 1;
};
  function dayUnique($b, $list) {
  global $choices, $filtered, $filtered2, $filtered3, $filtered4;
  $c = 0;
  foreach ($list as $x) {
  if ($x['day'] == $b['day']) {
  $c = $c + 1;
}
};
  return $c == 1;
};
  function monthWithUniqueDay($b, $list) {
  global $choices, $filtered, $filtered2, $filtered3, $filtered4;
  foreach ($list as $x) {
  if ($x['month'] == $b['month'] && dayUnique($x, $list)) {
  return true;
}
};
  return false;
};
  function bstr($b) {
  global $choices, $filtered, $filtered2, $filtered3, $filtered4;
  $months = ['', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
  return $months[$b['month']] . ' ' . _str($b['day']);
};
  $choices = [['month' => 5, 'day' => 15], ['month' => 5, 'day' => 16], ['month' => 5, 'day' => 19], ['month' => 6, 'day' => 17], ['month' => 6, 'day' => 18], ['month' => 7, 'day' => 14], ['month' => 7, 'day' => 16], ['month' => 8, 'day' => 14], ['month' => 8, 'day' => 15], ['month' => 8, 'day' => 17]];
  $filtered = [];
  foreach ($choices as $bd) {
  if (!monthUnique($bd, $choices)) {
  $filtered = array_merge($filtered, [$bd]);
}
}
  $filtered2 = [];
  foreach ($filtered as $bd) {
  if (!monthWithUniqueDay($bd, $filtered)) {
  $filtered2 = array_merge($filtered2, [$bd]);
}
}
  $filtered3 = [];
  foreach ($filtered2 as $bd) {
  if (dayUnique($bd, $filtered2)) {
  $filtered3 = array_merge($filtered3, [$bd]);
}
}
  $filtered4 = [];
  foreach ($filtered3 as $bd) {
  if (monthUnique($bd, $filtered3)) {
  $filtered4 = array_merge($filtered4, [$bd]);
}
}
  if (count($filtered4) == 1) {
  echo rtrim('Cheryl\'s birthday is ' . bstr($filtered4[0])), PHP_EOL;
} else {
  echo rtrim('Something went wrong!'), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
