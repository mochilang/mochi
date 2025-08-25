<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function pow2($exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
}
function proth($number) {
  if ($number < 1) {
  _panic('Input value must be > 0');
}
  if ($number == 1) {
  return 3;
}
  if ($number == 2) {
  return 5;
}
  $temp = intval((_intdiv($number, 3)));
  $pow = 1;
  $block_index = 1;
  while ($pow <= $temp) {
  $pow = $pow * 2;
  $block_index = $block_index + 1;
};
  $proth_list = [3, 5];
  $proth_index = 2;
  $increment = 3;
  $block = 1;
  while ($block < $block_index) {
  $i = 0;
  while ($i < $increment) {
  $next_val = pow2($block + 1) + $proth_list[$proth_index - 1];
  $proth_list = _append($proth_list, $next_val);
  $proth_index = $proth_index + 1;
  $i = $i + 1;
};
  $increment = $increment * 2;
  $block = $block + 1;
};
  return $proth_list[$number - 1];
}
function main() {
  $n = 1;
  while ($n <= 10) {
  $value = proth($n);
  echo rtrim('The ' . _str($n) . 'th Proth number: ' . _str($value)), PHP_EOL;
  $n = $n + 1;
};
}
main();
