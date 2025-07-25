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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function toUnsigned16($n) {
  $u = $n;
  if ($u < 0) {
  $u = $u + 65536;
}
  return $u % 65536;
};
  function bin16($n) {
  $u = toUnsigned16($n);
  $bits = '';
  $mask = 32768;
  for ($i = 0; $i < 16; $i++) {
  if ($u >= $mask) {
  $bits = $bits . '1';
  $u = $u - $mask;
} else {
  $bits = $bits . '0';
}
  $mask = intval((_intdiv($mask, 2)));
};
  return $bits;
};
  function bit_and($a, $b) {
  $ua = toUnsigned16($a);
  $ub = toUnsigned16($b);
  $res = 0;
  $bit = 1;
  for ($i = 0; $i < 16; $i++) {
  if ($ua % 2 == 1 && $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function bit_or($a, $b) {
  $ua = toUnsigned16($a);
  $ub = toUnsigned16($b);
  $res = 0;
  $bit = 1;
  for ($i = 0; $i < 16; $i++) {
  if ($ua % 2 == 1 || $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function bit_xor($a, $b) {
  $ua = toUnsigned16($a);
  $ub = toUnsigned16($b);
  $res = 0;
  $bit = 1;
  for ($i = 0; $i < 16; $i++) {
  $abit = $ua % 2;
  $bbit = $ub % 2;
  if (($abit == 1 && $bbit == 0) || ($abit == 0 && $bbit == 1)) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function bit_not($a) {
  $ua = toUnsigned16($a);
  return 65535 - $ua;
};
  function shl($a, $b) {
  $ua = toUnsigned16($a);
  $i = 0;
  while ($i < $b) {
  $ua = ($ua * 2) % 65536;
  $i = $i + 1;
};
  return $ua;
};
  function shr($a, $b) {
  $ua = toUnsigned16($a);
  $i = 0;
  while ($i < $b) {
  $ua = intval((_intdiv($ua, 2)));
  $i = $i + 1;
};
  return $ua;
};
  function las($a, $b) {
  return shl($a, $b);
};
  function ras($a, $b) {
  $val = $a;
  $i = 0;
  while ($i < $b) {
  if ($val >= 0) {
  $val = intval((_intdiv($val, 2)));
} else {
  $val = intval((_intdiv(($val - 1), 2)));
}
  $i = $i + 1;
};
  return toUnsigned16($val);
};
  function rol($a, $b) {
  $ua = toUnsigned16($a);
  $left = shl($ua, $b);
  $right = shr($ua, 16 - $b);
  return toUnsigned16($left + $right);
};
  function ror($a, $b) {
  $ua = toUnsigned16($a);
  $right = shr($ua, $b);
  $left = shl($ua, 16 - $b);
  return toUnsigned16($left + $right);
};
  function bitwise($a, $b) {
  echo rtrim('a:   ' . bin16($a)), PHP_EOL;
  echo rtrim('b:   ' . bin16($b)), PHP_EOL;
  echo rtrim('and: ' . bin16(bit_and($a, $b))), PHP_EOL;
  echo rtrim('or:  ' . bin16(bit_or($a, $b))), PHP_EOL;
  echo rtrim('xor: ' . bin16(bit_xor($a, $b))), PHP_EOL;
  echo rtrim('not: ' . bin16(bit_not($a))), PHP_EOL;
  if ($b < 0) {
  echo rtrim('Right operand is negative, but all shifts require an unsigned right operand (shift distance).'), PHP_EOL;
  return null;
}
  echo rtrim('shl: ' . bin16(shl($a, $b))), PHP_EOL;
  echo rtrim('shr: ' . bin16(shr($a, $b))), PHP_EOL;
  echo rtrim('las: ' . bin16(las($a, $b))), PHP_EOL;
  echo rtrim('ras: ' . bin16(ras($a, $b))), PHP_EOL;
  echo rtrim('rol: ' . bin16(rol($a, $b))), PHP_EOL;
  echo rtrim('ror: ' . bin16(ror($a, $b))), PHP_EOL;
};
  bitwise(-460, 6);
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
