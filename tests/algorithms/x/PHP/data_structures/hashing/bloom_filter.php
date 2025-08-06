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
$ascii = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
function mochi_ord($ch) {
  global $ascii;
  $i = 0;
  while ($i < strlen($ascii)) {
  if (substr($ascii, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
}
function new_bloom($size) {
  global $ascii;
  $bits = [];
  $i = 0;
  while ($i < $size) {
  $bits = _append($bits, 0);
  $i = $i + 1;
};
  return ['size' => $size, 'bits' => $bits];
}
function hash1($value, $size) {
  global $ascii;
  $h = 0;
  $i = 0;
  while ($i < strlen($value)) {
  $h = fmod(($h * 31 + mochi_ord(substr($value, $i, $i + 1 - $i))), $size);
  $i = $i + 1;
};
  return $h;
}
function hash2($value, $size) {
  global $ascii;
  $h = 0;
  $i = 0;
  while ($i < strlen($value)) {
  $h = fmod(($h * 131 + mochi_ord(substr($value, $i, $i + 1 - $i))), $size);
  $i = $i + 1;
};
  return $h;
}
function hash_positions($value, $size) {
  global $ascii;
  $h1 = hash1($value, $size);
  $h2 = hash2($value, $size);
  $res = [];
  $res = _append($res, $h1);
  $res = _append($res, $h2);
  return $res;
}
function bloom_add($b, $value) {
  global $ascii;
  $pos = hash_positions($value, $b['size']);
  $bits = $b['bits'];
  $i = 0;
  while ($i < count($pos)) {
  $idx = $b['size'] - 1 - $pos[$i];
  $bits[$idx] = 1;
  $i = $i + 1;
};
  return ['size' => $b['size'], 'bits' => $bits];
}
function bloom_exists($b, $value) {
  global $ascii;
  $pos = hash_positions($value, $b['size']);
  $i = 0;
  while ($i < count($pos)) {
  $idx = $b['size'] - 1 - $pos[$i];
  if ($b['bits'][$idx] != 1) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function bitstring($b) {
  global $ascii;
  $res = '';
  $i = 0;
  while ($i < $b['size']) {
  $res = $res . _str($b['bits'][$i]);
  $i = $i + 1;
};
  return $res;
}
function format_hash($b, $value) {
  global $ascii;
  $pos = hash_positions($value, $b['size']);
  $bits = [];
  $i = 0;
  while ($i < $b['size']) {
  $bits = _append($bits, 0);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($pos)) {
  $idx = $b['size'] - 1 - $pos[$i];
  $bits[$idx] = 1;
  $i = $i + 1;
};
  $res = '';
  $i = 0;
  while ($i < $b['size']) {
  $res = $res . _str($bits[$i]);
  $i = $i + 1;
};
  return $res;
}
function estimated_error_rate($b) {
  global $ascii;
  $ones = 0;
  $i = 0;
  while ($i < $b['size']) {
  if ($b['bits'][$i] == 1) {
  $ones = $ones + 1;
}
  $i = $i + 1;
};
  $frac = (floatval($ones)) / (floatval($b['size']));
  return $frac * $frac;
}
function any_in($b, $items) {
  global $ascii;
  $i = 0;
  while ($i < count($items)) {
  if (bloom_exists($b, $items[$i])) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function main() {
  global $ascii;
  $bloom = new_bloom(8);
  echo rtrim(bitstring($bloom)), PHP_EOL;
  echo rtrim(json_encode(bloom_exists($bloom, 'Titanic'), 1344)), PHP_EOL;
  $bloom = bloom_add($bloom, 'Titanic');
  echo rtrim(bitstring($bloom)), PHP_EOL;
  echo rtrim(json_encode(bloom_exists($bloom, 'Titanic'), 1344)), PHP_EOL;
  $bloom = bloom_add($bloom, 'Avatar');
  echo rtrim(json_encode(bloom_exists($bloom, 'Avatar'), 1344)), PHP_EOL;
  echo rtrim(format_hash($bloom, 'Avatar')), PHP_EOL;
  echo rtrim(bitstring($bloom)), PHP_EOL;
  $not_present = ['The Godfather', 'Interstellar', 'Parasite', 'Pulp Fiction'];
  $i = 0;
  while ($i < count($not_present)) {
  $film = $not_present[$i];
  echo rtrim($film . ':' . format_hash($bloom, $film)), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(json_encode(any_in($bloom, $not_present), 1344)), PHP_EOL;
  echo rtrim(json_encode(bloom_exists($bloom, 'Ratatouille'), 1344)), PHP_EOL;
  echo rtrim(format_hash($bloom, 'Ratatouille')), PHP_EOL;
  echo rtrim(_str(estimated_error_rate($bloom))), PHP_EOL;
  $bloom = bloom_add($bloom, 'The Godfather');
  echo rtrim(_str(estimated_error_rate($bloom))), PHP_EOL;
  echo rtrim(bitstring($bloom)), PHP_EOL;
}
main();
