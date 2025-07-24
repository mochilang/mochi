<?php
ini_set('memory_limit', '-1');
function _sha256($bs) {
    $str = '';
    foreach ($bs as $b) { $str .= chr($b); }
    return array_values(unpack('C*', hash('sha256', $str, true)));
}
function indexOf($s, $ch) {
  global $set58, $doubleSHA256, $computeChecksum, $validA58;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function set58($addr) {
  global $indexOf, $doubleSHA256, $computeChecksum, $validA58;
  $tmpl = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
  $a = [];
  $i = 0;
  while ($i < 25) {
  $a = array_merge($a, [0]);
  $i = $i + 1;
};
  $idx = 0;
  while ($idx < strlen($addr)) {
  $ch = substr($addr, $idx, $idx + 1 - $idx);
  $c = indexOf($tmpl, $ch);
  if ($c < 0) {
  return [];
}
  $j = 24;
  while ($j >= 0) {
  $c = $c + 58 * $a[$j];
  $a[$j] = $c % 256;
  $c = intval((intdiv($c, 256)));
  $j = $j - 1;
};
  if ($c > 0) {
  return [];
}
  $idx = $idx + 1;
};
  return $a;
}
function doubleSHA256($bs) {
  global $indexOf, $set58, $computeChecksum, $validA58;
  $first = _sha256($bs);
  return _sha256($first);
}
function computeChecksum($a) {
  global $indexOf, $set58, $doubleSHA256, $validA58;
  $hash = doubleSHA256(array_slice($a, 0, 21 - 0));
  return array_slice($hash, 0, 4 - 0);
}
function validA58($addr) {
  global $indexOf, $set58, $doubleSHA256, $computeChecksum;
  $a = set58($addr);
  if (count($a) != 25) {
  return false;
}
  if ($a[0] != 0) {
  return false;
}
  $sum = computeChecksum($a);
  $i = 0;
  while ($i < 4) {
  if ($a[21 + $i] != $sum[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
echo rtrim(json_encode(validA58('1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i'), 1344)), PHP_EOL;
echo rtrim(json_encode(validA58('17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j'), 1344)), PHP_EOL;
