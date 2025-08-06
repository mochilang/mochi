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
  $square = [['a', 'b', 'c', 'd', 'e'], ['f', 'g', 'h', 'i', 'k'], ['l', 'm', 'n', 'o', 'p'], ['q', 'r', 's', 't', 'u'], ['v', 'w', 'x', 'y', 'z']];
  function letter_to_numbers($letter) {
  global $square;
  $i = 0;
  while ($i < count($square)) {
  $j = 0;
  while ($j < count($square[$i])) {
  if ($square[$i][$j] == $letter) {
  return [$i + 1, $j + 1];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return [0, 0];
};
  function numbers_to_letter($index1, $index2) {
  global $square;
  return $square[$index1 - 1][$index2 - 1];
};
  function char_to_int($ch) {
  global $square;
  if ($ch == '1') {
  return 1;
}
  if ($ch == '2') {
  return 2;
}
  if ($ch == '3') {
  return 3;
}
  if ($ch == '4') {
  return 4;
}
  if ($ch == '5') {
  return 5;
}
  return 0;
};
  function encode($message) {
  global $square;
  $message = strtolower($message);
  $encoded = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, $i + 1 - $i);
  if ($ch == 'j') {
  $ch = 'i';
}
  if ($ch != ' ') {
  $nums = letter_to_numbers($ch);
  $encoded = $encoded . _str($nums[0]) . _str($nums[1]);
} else {
  $encoded = $encoded . ' ';
}
  $i = $i + 1;
};
  return $encoded;
};
  function decode($message) {
  global $square;
  $decoded = '';
  $i = 0;
  while ($i < strlen($message)) {
  if (substr($message, $i, $i + 1 - $i) == ' ') {
  $decoded = $decoded . ' ';
  $i = $i + 1;
} else {
  $index1 = char_to_int(substr($message, $i, $i + 1 - $i));
  $index2 = char_to_int(substr($message, $i + 1, $i + 1 + 1 - ($i + 1)));
  $letter = numbers_to_letter($index1, $index2);
  $decoded = $decoded . $letter;
  $i = $i + 2;
}
};
  return $decoded;
};
  echo rtrim(encode('test message')), PHP_EOL;
  echo rtrim(encode('Test Message')), PHP_EOL;
  echo rtrim(decode('44154344 32154343112215')), PHP_EOL;
  echo rtrim(decode('4415434432154343112215')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
