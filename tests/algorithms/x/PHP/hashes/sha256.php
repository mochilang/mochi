<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _sha256($bs) {
    $bin = '';
    foreach ($bs as $b) { $bin .= chr($b); }
    $hash = hash('sha256', $bin, true);
    return array_values(unpack('C*', $hash));
}
$HEX = '0123456789abcdef';
function byte_to_hex($b) {
  global $HEX;
  $hi = _intdiv($b, 16);
  $lo = $b % 16;
  return substr($HEX, $hi, $hi + 1 - $hi) . substr($HEX, $lo, $lo + 1 - $lo);
}
function bytes_to_hex($bs) {
  global $HEX;
  $res = '';
  $i = 0;
  while ($i < count($bs)) {
  $res = $res . byte_to_hex($bs[$i]);
  $i = $i + 1;
};
  return $res;
}
echo rtrim(bytes_to_hex(_sha256('Python'))), PHP_EOL;
echo rtrim(bytes_to_hex(_sha256('hello world'))), PHP_EOL;
