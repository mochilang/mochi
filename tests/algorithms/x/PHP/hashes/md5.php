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
function to_little_endian($s) {
  global $MOD, $ASCII;
  if (strlen($s) != 32) {
  $panic('Input must be of length 32');
}
  return substr($s, 24, 32 - 24) . substr($s, 16, 24 - 16) . substr($s, 8, 16 - 8) . substr($s, 0, 8 - 0);
}
function int_to_bits($n, $width) {
  global $MOD, $ASCII;
  $bits = '';
  $num = $n;
  while ($num > 0) {
  $bits = _str($num % 2) . $bits;
  $num = _intdiv($num, 2);
};
  while (strlen($bits) < $width) {
  $bits = '0' . $bits;
};
  if (strlen($bits) > $width) {
  $bits = substr($bits, strlen($bits) - $width, strlen($bits) - (strlen($bits) - $width));
}
  return $bits;
}
function bits_to_int($bits) {
  global $MOD, $ASCII;
  $num = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  if (substr($bits, $i, $i + 1 - $i) == '1') {
  $num = $num * 2 + 1;
} else {
  $num = $num * 2;
}
  $i = $i + 1;
};
  return $num;
}
function to_hex($n) {
  global $MOD, $ASCII;
  $digits = '0123456789abcdef';
  if ($n == 0) {
  return '0';
}
  $num = $n;
  $s = '';
  while ($num > 0) {
  $d = $num % 16;
  $s = substr($digits, $d, $d + 1 - $d) . $s;
  $num = _intdiv($num, 16);
};
  return $s;
}
function reformat_hex($i) {
  global $MOD, $ASCII;
  if ($i < 0) {
  $panic('Input must be non-negative');
}
  $hex = to_hex($i);
  while (strlen($hex) < 8) {
  $hex = '0' . $hex;
};
  if (strlen($hex) > 8) {
  $hex = substr($hex, strlen($hex) - 8, strlen($hex) - (strlen($hex) - 8));
}
  $le = '';
  $j = strlen($hex) - 2;
  while ($j >= 0) {
  $le = $le . substr($hex, $j, $j + 2 - $j);
  $j = $j - 2;
};
  return $le;
}
function preprocess($message) {
  global $MOD, $ASCII;
  $bit_string = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, $i + 1 - $i);
  $bit_string = $bit_string . int_to_bits(mochi_ord($ch), 8);
  $i = $i + 1;
};
  $start_len = int_to_bits(strlen($bit_string), 64);
  $bit_string = $bit_string . '1';
  while (fmod(strlen($bit_string), 512) != 448) {
  $bit_string = $bit_string . '0';
};
  $bit_string = $bit_string . to_little_endian(substr($start_len, 32, 64 - 32)) . to_little_endian(substr($start_len, 0, 32 - 0));
  return $bit_string;
}
function get_block_words($bit_string) {
  global $MOD, $ASCII;
  if (fmod(strlen($bit_string), 512) != 0) {
  $panic('Input must have length that\'s a multiple of 512');
}
  $blocks = [];
  $pos = 0;
  while ($pos < strlen($bit_string)) {
  $block = [];
  $i = 0;
  while ($i < 512) {
  $part = substr($bit_string, $pos + $i, $pos + $i + 32 - ($pos + $i));
  $word = bits_to_int(to_little_endian($part));
  $block = _append($block, $word);
  $i = $i + 32;
};
  $blocks = _append($blocks, $block);
  $pos = $pos + 512;
};
  return $blocks;
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
  if (($abit + $bbit) % 2 == 1) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
  $i = $i + 1;
};
  return $res;
}
function not_32($i) {
  global $MOD, $ASCII;
  if ($i < 0) {
  $panic('Input must be non-negative');
}
  return 4294967295 - $i;
}
function sum_32($a, $b) {
  global $MOD, $ASCII;
  return ($a + $b) % $MOD;
}
function lshift($num, $k) {
  global $MOD, $ASCII;
  $result = $num % $MOD;
  $i = 0;
  while ($i < $k) {
  $result = ($result * 2) % $MOD;
  $i = $i + 1;
};
  return $result;
}
function rshift($num, $k) {
  global $MOD, $ASCII;
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = _intdiv($result, 2);
  $i = $i + 1;
};
  return $result;
}
function left_rotate_32($i, $shift) {
  global $MOD, $ASCII;
  if ($i < 0) {
  $panic('Input must be non-negative');
}
  if ($shift < 0) {
  $panic('Shift must be non-negative');
}
  $left = lshift($i, $shift);
  $right = rshift($i, 32 - $shift);
  return ($left + $right) % $MOD;
}
function md5_me($message) {
  global $MOD, $ASCII;
  $bit_string = preprocess($message);
  $added_consts = [3614090360, 3905402710, 606105819, 3250441966, 4118548399, 1200080426, 2821735955, 4249261313, 1770035416, 2336552879, 4294925233, 2304563134, 1804603682, 4254626195, 2792965006, 1236535329, 4129170786, 3225465664, 643717713, 3921069994, 3593408605, 38016083, 3634488961, 3889429448, 568446438, 3275163606, 4107603335, 1163531501, 2850285829, 4243563512, 1735328473, 2368359562, 4294588738, 2272392833, 1839030562, 4259657740, 2763975236, 1272893353, 4139469664, 3200236656, 681279174, 3936430074, 3572445317, 76029189, 3654602809, 3873151461, 530742520, 3299628645, 4096336452, 1126891415, 2878612391, 4237533241, 1700485571, 2399980690, 4293915773, 2240044497, 1873313359, 4264355552, 2734768916, 1309151649, 4149444226, 3174756917, 718787259, 3951481745];
  $shift_amounts = [7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21];
  $a0 = 1732584193;
  $b0 = 4023233417;
  $c0 = 2562383102;
  $d0 = 271733878;
  $blocks = get_block_words($bit_string);
  $bi = 0;
  while ($bi < count($blocks)) {
  $block = $blocks[$bi];
  $a = $a0;
  $b = $b0;
  $c = $c0;
  $d = $d0;
  $i = 0;
  while ($i < 64) {
  $f = 0;
  $g = 0;
  if ($i <= 15) {
  $f = bit_xor($d, bit_and($b, bit_xor($c, $d)));
  $g = $i;
} else {
  if ($i <= 31) {
  $f = bit_xor($c, bit_and($d, bit_xor($b, $c)));
  $g = (5 * $i + 1) % 16;
} else {
  if ($i <= 47) {
  $f = bit_xor(bit_xor($b, $c), $d);
  $g = (3 * $i + 5) % 16;
} else {
  $f = bit_xor($c, bit_or($b, not_32($d)));
  $g = (7 * $i) % 16;
};
};
}
  $f = sum_32($f, $a);
  $f = sum_32($f, $added_consts[$i]);
  $f = sum_32($f, $block[$g]);
  $rotated = left_rotate_32($f, $shift_amounts[$i]);
  $new_b = sum_32($b, $rotated);
  $a = $d;
  $d = $c;
  $c = $b;
  $b = $new_b;
  $i = $i + 1;
};
  $a0 = sum_32($a0, $a);
  $b0 = sum_32($b0, $b);
  $c0 = sum_32($c0, $c);
  $d0 = sum_32($d0, $d);
  $bi = $bi + 1;
};
  $digest = reformat_hex($a0) . reformat_hex($b0) . reformat_hex($c0) . reformat_hex($d0);
  return $digest;
}
