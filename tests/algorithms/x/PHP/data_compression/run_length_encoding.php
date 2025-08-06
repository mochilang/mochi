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
function run_length_encode($text) {
  global $example1, $encoded1, $example2, $encoded2, $example3, $encoded3;
  if (strlen($text) == 0) {
  return '';
}
  $encoded = '';
  $count = 1;
  $i = 0;
  while ($i < strlen($text)) {
  if ($i + 1 < strlen($text) && substr($text, $i, $i + 1 - $i) == substr($text, $i + 1, $i + 1 + 1 - ($i + 1))) {
  $count = $count + 1;
} else {
  $encoded = $encoded . substr($text, $i, $i + 1 - $i) . _str($count);
  $count = 1;
}
  $i = $i + 1;
};
  return $encoded;
}
function run_length_decode($encoded) {
  global $example1, $encoded1, $example2, $encoded2, $example3, $encoded3;
  $res = '';
  $i = 0;
  while ($i < strlen($encoded)) {
  $ch = substr($encoded, $i, $i + 1 - $i);
  $i = $i + 1;
  $num_str = '';
  while ($i < strlen($encoded) && substr($encoded, $i, $i + 1 - $i) >= '0' && substr($encoded, $i, $i + 1 - $i) <= '9') {
  $num_str = $num_str . substr($encoded, $i, $i + 1 - $i);
  $i = $i + 1;
};
  $count = intval($num_str);
  $j = 0;
  while ($j < $count) {
  $res = $res . $ch;
  $j = $j + 1;
};
};
  return $res;
}
$example1 = 'AAAABBBCCDAA';
$encoded1 = run_length_encode($example1);
echo rtrim($encoded1), PHP_EOL;
echo rtrim(run_length_decode($encoded1)), PHP_EOL;
$example2 = 'A';
$encoded2 = run_length_encode($example2);
echo rtrim($encoded2), PHP_EOL;
echo rtrim(run_length_decode($encoded2)), PHP_EOL;
$example3 = 'AAADDDDDDFFFCCCAAVVVV';
$encoded3 = run_length_encode($example3);
echo rtrim($encoded3), PHP_EOL;
echo rtrim(run_length_decode($encoded3)), PHP_EOL;
