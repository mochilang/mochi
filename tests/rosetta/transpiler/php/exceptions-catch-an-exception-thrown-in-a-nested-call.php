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
$__start_mem = memory_get_usage();
$__start = _now();
  $bazCall = 0;
  function baz() {
  global $bazCall;
  $bazCall = $bazCall + 1;
  echo rtrim('baz: start'), PHP_EOL;
  if ($bazCall == 1) {
  echo rtrim('baz: raising U0'), PHP_EOL;
  return 'U0';
}
  if ($bazCall == 2) {
  echo rtrim('baz: raising U1'), PHP_EOL;
  return 'U1';
}
  echo rtrim('baz: end'), PHP_EOL;
  return '';
};
  function bar() {
  global $bazCall;
  echo rtrim('bar: start'), PHP_EOL;
  $err = baz();
  if (strlen($err) > 0) {
  return $err;
}
  echo rtrim('bar: end'), PHP_EOL;
  return '';
};
  function foo() {
  global $bazCall;
  echo rtrim('foo: start'), PHP_EOL;
  $err = bar();
  if ($err == 'U0') {
  echo rtrim('foo: caught U0'), PHP_EOL;
} else {
  if (strlen($err) > 0) {
  return $err;
};
}
  $err = bar();
  if ($err == 'U0') {
  echo rtrim('foo: caught U0'), PHP_EOL;
} else {
  if (strlen($err) > 0) {
  return $err;
};
}
  echo rtrim('foo: end'), PHP_EOL;
  return '';
};
  function main() {
  global $bazCall;
  echo rtrim('main: start'), PHP_EOL;
  $err = foo();
  if (strlen($err) > 0) {
  echo rtrim('main: unhandled ' . $err), PHP_EOL;
} else {
  echo rtrim('main: success'), PHP_EOL;
}
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
