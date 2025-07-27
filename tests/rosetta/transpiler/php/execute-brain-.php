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
  function mochi_chr($n) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - ($n - 97));
}
  if ($n == 32) {
  return ' ';
}
  if ($n == 33) {
  return '!';
}
  if ($n == 44) {
  return ',';
}
  if ($n == 13) {
  return '';
}
  if ($n == 10) {
  return '
';
}
  return '?';
};
  function bf($dLen, $code) {
  $ds = [];
  for ($i = 0; $i < $dLen; $i++) {
  $ds = array_merge($ds, [0]);
};
  $dp = 0;
  $ip = 0;
  $out = '';
  while ($ip < strlen($code)) {
  $ch = substr($code, $ip, $ip + 1 - $ip);
  if ($ch == '>') {
  $dp = $dp + 1;
} else {
  if ($ch == '<') {
  $dp = $dp - 1;
} else {
  if ($ch == '+') {
  $ds[$dp] = $ds[$dp] + 1;
} else {
  if ($ch == '-') {
  $ds[$dp] = $ds[$dp] - 1;
} else {
  if ($ch == '.') {
  $out = $out . mochi_chr($ds[$dp]);
} else {
  if ($ch == ',') {
} else {
  if ($ch == '[') {
  if ($ds[$dp] == 0) {
  $nc = 1;
  while ($nc > 0) {
  $ip = $ip + 1;
  $cc = substr($code, $ip, $ip + 1 - $ip);
  if ($cc == '[') {
  $nc = $nc + 1;
} else {
  if ($cc == ']') {
  $nc = $nc - 1;
};
}
};
};
} else {
  if ($ch == ']') {
  if ($ds[$dp] != 0) {
  $nc = 1;
  while ($nc > 0) {
  $ip = $ip - 1;
  $cc = substr($code, $ip, $ip + 1 - $ip);
  if ($cc == ']') {
  $nc = $nc + 1;
} else {
  if ($cc == '[') {
  $nc = $nc - 1;
};
}
};
};
};
};
};
};
};
};
};
}
  $ip = $ip + 1;
};
  return $out;
};
  function main() {
  $prog = '++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++
' . '++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>
' . '>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.
' . '<+++++++.--------.<<<<<+.<+++.---.';
  $out = bf(10, $prog);
  echo rtrim($out), PHP_EOL;
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
