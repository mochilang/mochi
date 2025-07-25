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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function skipWS(&$p) {
  $i = $p['pos'];
  while ($i < _len($p['expr']) && substr($p['expr'], $i, $i + 1 - $i) == ' ') {
  $i = $i + 1;
};
  $p['pos'] = $i;
  return $p;
};
  function mochi_parseIntStr($str) {
  $i = 0;
  $n = 0;
  while ($i < strlen($str)) {
  $n = $n * 10 + (ord(substr($str, $i, $i + 1 - $i))) - 48;
  $i = $i + 1;
};
  return $n;
};
  function parseNumber(&$p) {
  $p = skipWS($p);
  $start = $p['pos'];
  while ($p['pos'] < _len($p['expr'])) {
  $ch = substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']);
  if ($ch >= '0' && $ch <= '9') {
  $p['pos'] = $p['pos'] + 1;
} else {
  break;
}
};
  $token = substr($p['expr'], $start, $p['pos'] - $start);
  return ['v' => parseIntStr($token, 10), 'p' => $p];
};
  function parseFactor(&$p) {
  $p = skipWS($p);
  if ($p['pos'] < _len($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == '(') {
  $p['pos'] = $p['pos'] + 1;
  $r = parseExpr($p);
  $v = $r['v'];
  $p = $r['p'];
  $p = skipWS($p);
  if ($p['pos'] < _len($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == ')') {
  $p['pos'] = $p['pos'] + 1;
};
  return ['v' => $v, 'p' => $p];
}
  if ($p['pos'] < _len($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == '-') {
  $p['pos'] = $p['pos'] + 1;
  $r = parseFactor($p);
  $v = $r['v'];
  $p = $r['p'];
  return ['v' => -$v, 'p' => $p];
}
  return parseNumber($p);
};
  function powInt($base, $exp) {
  $r = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $r = $r * $b;
}
  $b = $b * $b;
  $e = $e / intval(2);
};
  return $r;
};
  function parsePower(&$p) {
  $r = parseFactor($p);
  $v = $r['v'];
  $p = $r['p'];
  while (true) {
  $p = skipWS($p);
  if ($p['pos'] < _len($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == '^') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parseFactor($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = powInt($v, $rhs);
} else {
  break;
}
};
  return ['v' => $v, 'p' => $p];
};
  function parseTerm(&$p) {
  $r = parsePower($p);
  $v = $r['v'];
  $p = $r['p'];
  while (true) {
  $p = skipWS($p);
  if ($p['pos'] < _len($p['expr'])) {
  $op = substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']);
  if ($op == '*') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parsePower($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v * $rhs;
  continue;
};
  if ($op == '/') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parsePower($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v / intval($rhs);
  continue;
};
}
  break;
};
  return ['v' => $v, 'p' => $p];
};
  function parseExpr(&$p) {
  $r = parseTerm($p);
  $v = $r['v'];
  $p = $r['p'];
  while (true) {
  $p = skipWS($p);
  if ($p['pos'] < _len($p['expr'])) {
  $op = substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']);
  if ($op == '+') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parseTerm($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v + $rhs;
  continue;
};
  if ($op == '-') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parseTerm($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v - $rhs;
  continue;
};
}
  break;
};
  return ['v' => $v, 'p' => $p];
};
  function evalExpr($expr) {
  $p = ['expr' => $expr, 'pos' => 0];
  $r = parseExpr($p);
  return $r['v'];
};
  function main() {
  $expr = '2*(3-1)+2*5';
  echo rtrim($expr . ' = ' . _str(evalExpr($expr))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
