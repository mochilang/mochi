<?php
ini_set('memory_limit', '-1');
function toUnsigned16($n) {
  global $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $rol, $ror, $bitwise;
  $u = $n;
  if ($u < 0) {
  $u = $u + 65536;
}
  return $u % 65536;
}
function bin16($n) {
  global $toUnsigned16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $rol, $ror, $bitwise;
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
  $mask = intval((intdiv($mask, 2)));
};
  return $bits;
}
function bit_and($a, $b) {
  global $toUnsigned16, $bin16, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $rol, $ror, $bitwise;
  $ua = toUnsigned16($a);
  $ub = toUnsigned16($b);
  $res = 0;
  $bit = 1;
  for ($i = 0; $i < 16; $i++) {
  if ($ua % 2 == 1 && $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((intdiv($ua, 2)));
  $ub = intval((intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
}
function bit_or($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $rol, $ror, $bitwise;
  $ua = toUnsigned16($a);
  $ub = toUnsigned16($b);
  $res = 0;
  $bit = 1;
  for ($i = 0; $i < 16; $i++) {
  if ($ua % 2 == 1 || $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((intdiv($ua, 2)));
  $ub = intval((intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
}
function bit_xor($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_not, $shl, $shr, $las, $ras, $rol, $ror, $bitwise;
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
  $ua = intval((intdiv($ua, 2)));
  $ub = intval((intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
}
function bit_not($a) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $shl, $shr, $las, $ras, $rol, $ror, $bitwise;
  $ua = toUnsigned16($a);
  return 65535 - $ua;
}
function shl($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shr, $las, $ras, $rol, $ror, $bitwise;
  $ua = toUnsigned16($a);
  $i = 0;
  while ($i < $b) {
  $ua = ($ua * 2) % 65536;
  $i = $i + 1;
};
  return $ua;
}
function shr($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $las, $ras, $rol, $ror, $bitwise;
  $ua = toUnsigned16($a);
  $i = 0;
  while ($i < $b) {
  $ua = intval((intdiv($ua, 2)));
  $i = $i + 1;
};
  return $ua;
}
function las($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $ras, $rol, $ror, $bitwise;
  return shl($a, $b);
}
function ras($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $rol, $ror, $bitwise;
  $val = $a;
  $i = 0;
  while ($i < $b) {
  if ($val >= 0) {
  $val = intval((intdiv($val, 2)));
} else {
  $val = intval((intdiv(($val - 1), 2)));
}
  $i = $i + 1;
};
  return toUnsigned16($val);
}
function rol($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $ror, $bitwise;
  $ua = toUnsigned16($a);
  $left = shl($ua, $b);
  $right = shr($ua, 16 - $b);
  return toUnsigned16($left + $right);
}
function ror($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $rol, $bitwise;
  $ua = toUnsigned16($a);
  $right = shr($ua, $b);
  $left = shl($ua, 16 - $b);
  return toUnsigned16($left + $right);
}
function bitwise($a, $b) {
  global $toUnsigned16, $bin16, $bit_and, $bit_or, $bit_xor, $bit_not, $shl, $shr, $las, $ras, $rol, $ror;
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
}
bitwise(-460, 6);
