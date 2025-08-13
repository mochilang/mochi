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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function absf($x) {
  global $r1, $r2, $r3, $r4, $r5;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function pow10($n) {
  global $r1, $r2, $r3, $r4, $r5;
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
}
function round_to($x, $n) {
  global $r1, $r2, $r3, $r4, $r5;
  $m = pow10($n);
  return floor($x * $m + 0.5) / $m;
}
function electric_power($voltage, $current, $power) {
  global $r1, $r2, $r3, $r4, $r5;
  $zeros = 0;
  if ($voltage == 0.0) {
  $zeros = $zeros + 1;
}
  if ($current == 0.0) {
  $zeros = $zeros + 1;
}
  if ($power == 0.0) {
  $zeros = $zeros + 1;
}
  if ($zeros != 1) {
  _panic('Exactly one argument must be 0');
} else {
  if ($power < 0.0) {
  _panic('Power cannot be negative in any electrical/electronics system');
} else {
  if ($voltage == 0.0) {
  return ['name' => 'voltage', 'value' => $power / $current];
} else {
  if ($current == 0.0) {
  return ['name' => 'current', 'value' => $power / $voltage];
} else {
  if ($power == 0.0) {
  $p = absf($voltage * $current);
  return ['name' => 'power', 'value' => round_to($p, 2)];
} else {
  _panic('Unhandled case');
};
};
};
};
}
}
function str_result($r) {
  global $r1, $r2, $r3, $r4, $r5;
  return 'Result(name=\'' . $r['name'] . '\', value=' . _str($r['value']) . ')';
}
$r1 = electric_power(0.0, 2.0, 5.0);
echo rtrim(str_result($r1)), PHP_EOL;
$r2 = electric_power(2.0, 2.0, 0.0);
echo rtrim(str_result($r2)), PHP_EOL;
$r3 = electric_power(-2.0, 3.0, 0.0);
echo rtrim(str_result($r3)), PHP_EOL;
$r4 = electric_power(2.2, 2.2, 0.0);
echo rtrim(str_result($r4)), PHP_EOL;
$r5 = electric_power(2.0, 0.0, 6.0);
echo rtrim(str_result($r5)), PHP_EOL;
