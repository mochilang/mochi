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
$__start_mem = memory_get_usage();
$__start = _now();
  function starts_with($s, $prefix) {
  global $phone;
  if (strlen($s) < strlen($prefix)) {
  return false;
}
  return substr($s, 0, strlen($prefix)) == $prefix;
};
  function all_digits($s) {
  global $phone;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function is_sri_lankan_phone_number($phone) {
  $p = $phone;
  if (starts_with($p, '+94')) {
  $p = substr($p, 3, strlen($p) - 3);
} else {
  if (starts_with($p, '0094')) {
  $p = substr($p, 4, strlen($p) - 4);
} else {
  if (starts_with($p, '94')) {
  $p = substr($p, 2, strlen($p) - 2);
} else {
  if (starts_with($p, '0')) {
  $p = substr($p, 1, strlen($p) - 1);
} else {
  return false;
};
};
};
}
  if (strlen($p) != 9 && strlen($p) != 10) {
  return false;
}
  if (substr($p, 0, 0 + 1) != '7') {
  return false;
}
  $second = substr($p, 1, 1 + 1 - 1);
  $allowed = ['0', '1', '2', '4', '5', '6', '7', '8'];
  if (!(in_array($second, $allowed))) {
  return false;
}
  $idx = 2;
  if (strlen($p) == 10) {
  $sep = substr($p, 2, 2 + 1 - 2);
  if ($sep != '-' && $sep != ' ') {
  return false;
};
  $idx = 3;
}
  if (strlen($p) - $idx != 7) {
  return false;
}
  $rest = substr($p, $idx, strlen($p) - $idx);
  return all_digits($rest);
};
  $phone = '0094702343221';
  echo rtrim(_str(is_sri_lankan_phone_number($phone))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
