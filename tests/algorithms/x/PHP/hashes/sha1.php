<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$MOD = 4294967296;
$ASCII = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
function mochi_ord($ch) {
  global $MOD, $ASCII;
  $i = 0;
  while ($i < strlen($ASCII)) {
  if (substr($ASCII, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
}
function pow2($n) {
  global $MOD, $ASCII;
  $res = 1;
  $i = 0;
  while ($i < $n) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
}
function bit_and($a, $b) {
  global $MOD, $ASCII;
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  $i = 0;
  while ($i < 32) {
  if (($x % 2 == 1) && ($y % 2 == 1)) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
  $i = $i + 1;
};
  return $res;
}
function bit_or($a, $b) {
  global $MOD, $ASCII;
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  $i = 0;
  while ($i < 32) {
  $abit = $x % 2;
  $bbit = $y % 2;
  if ($abit == 1 || $bbit == 1) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
  $i = $i + 1;
};
  return $res;
}
function bit_xor($a, $b) {
  global $MOD, $ASCII;
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  $i = 0;
  while ($i < 32) {
  $abit = $x % 2;
  $bbit = $y % 2;
  if (($abit == 1 && $bbit == 0) || ($abit == 0 && $bbit == 1)) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
  $i = $i + 1;
};
  return $res;
}
function bit_not($a) {
  global $MOD, $ASCII;
  return ($MOD - 1) - $a;
}
function rotate_left($n, $b) {
  global $MOD, $ASCII;
  $left = fmod(($n * pow2($b)), $MOD);
  $right = $n / pow2(32 - $b);
  return ($left + $right) % $MOD;
}
function to_hex32($n) {
  global $MOD, $ASCII;
  $digits = '0123456789abcdef';
  $num = $n;
  $s = '';
  if ($num == 0) {
  $s = '0';
}
  while ($num > 0) {
  $d = $num % 16;
  $s = substr($digits, $d, $d + 1 - $d) . $s;
  $num = _intdiv($num, 16);
};
  while (strlen($s) < 8) {
  $s = '0' . $s;
};
  if (strlen($s) > 8) {
  $s = substr($s, strlen($s) - 8, strlen($s) - (strlen($s) - 8));
}
  return $s;
}
function sha1($message) {
  global $MOD, $ASCII;
  $bytes = [];
  $i = 0;
  while ($i < strlen($message)) {
  $bytes = _append($bytes, mochi_ord($message[$i]));
  $i = $i + 1;
};
  $bytes = _append($bytes, 128);
  while (fmod((count($bytes) + 8), 64) != 0) {
  $bytes = _append($bytes, 0);
};
  $bit_len = strlen($message) * 8;
  $len_bytes = [0, 0, 0, 0, 0, 0, 0, 0];
  $bl = $bit_len;
  $k = 7;
  while ($k >= 0) {
  $len_bytes[$k] = $bl % 256;
  $bl = _intdiv($bl, 256);
  $k = $k - 1;
};
  $j = 0;
  while ($j < 8) {
  $bytes = _append($bytes, $len_bytes[$j]);
  $j = $j + 1;
};
  $blocks = [];
  $pos = 0;
  while ($pos < count($bytes)) {
  $block = [];
  $j2 = 0;
  while ($j2 < 64) {
  $block = _append($block, $bytes[$pos + $j2]);
  $j2 = $j2 + 1;
};
  $blocks = _append($blocks, $block);
  $pos = $pos + 64;
};
  $h0 = 1732584193;
  $h1 = 4023233417;
  $h2 = 2562383102;
  $h3 = 271733878;
  $h4 = 3285377520;
  $bindex = 0;
  while ($bindex < count($blocks)) {
  $block = $blocks[$bindex];
  $w = [];
  $t = 0;
  while ($t < 16) {
  $j3 = $t * 4;
  $word = ((($block[$j3] * 256 + $block[$j3 + 1]) * 256 + $block[$j3 + 2]) * 256 + $block[$j3 + 3]);
  $w = _append($w, $word);
  $t = $t + 1;
};
  while ($t < 80) {
  $tmp = bit_xor(bit_xor(bit_xor($w[$t - 3], $w[$t - 8]), $w[$t - 14]), $w[$t - 16]);
  $w = _append($w, rotate_left($tmp, 1));
  $t = $t + 1;
};
  $a = $h0;
  $b = $h1;
  $c = $h2;
  $d = $h3;
  $e = $h4;
  $i2 = 0;
  while ($i2 < 80) {
  $f = 0;
  $kconst = 0;
  if ($i2 < 20) {
  $f = bit_or(bit_and($b, $c), bit_and(bit_not($b), $d));
  $kconst = 1518500249;
} else {
  if ($i2 < 40) {
  $f = bit_xor(bit_xor($b, $c), $d);
  $kconst = 1859775393;
} else {
  if ($i2 < 60) {
  $f = bit_or(bit_or(bit_and($b, $c), bit_and($b, $d)), bit_and($c, $d));
  $kconst = 2400959708;
} else {
  $f = bit_xor(bit_xor($b, $c), $d);
  $kconst = 3395469782;
};
};
}
  $temp = fmod((rotate_left($a, 5) + $f + $e + $kconst + $w[$i2]), $MOD);
  $e = $d;
  $d = $c;
  $c = rotate_left($b, 30);
  $b = $a;
  $a = $temp;
  $i2 = $i2 + 1;
};
  $h0 = ($h0 + $a) % $MOD;
  $h1 = ($h1 + $b) % $MOD;
  $h2 = ($h2 + $c) % $MOD;
  $h3 = ($h3 + $d) % $MOD;
  $h4 = ($h4 + $e) % $MOD;
  $bindex = $bindex + 1;
};
  return to_hex32($h0) . to_hex32($h1) . to_hex32($h2) . to_hex32($h3) . to_hex32($h4);
}
function main() {
  global $MOD, $ASCII;
  echo rtrim(sha1('Test String')), PHP_EOL;
}
main();
