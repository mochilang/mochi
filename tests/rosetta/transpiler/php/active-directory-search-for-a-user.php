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
  function search_user($directory, $username) {
  return $directory[$username];
};
  function main() {
  $client = ['Base' => 'dc=example,dc=com', 'Host' => 'ldap.example.com', 'Port' => 389, 'GroupFilter' => '(memberUid=%s)'];
  $directory = ['username' => ['admins', 'users'], 'john' => ['users']];
  $groups = search_user($directory, 'username');
  if (count($groups) > 0) {
  $out = 'Groups: [';
  $i = 0;
  while ($i < count($groups)) {
  $out = $out . '"' . $groups[$i] . '"';
  if ($i < count($groups) - 1) {
  $out = $out . ', ';
}
  $i = $i + 1;
};
  $out = $out . ']';
  echo rtrim($out), PHP_EOL;
} else {
  echo rtrim('User not found'), PHP_EOL;
}
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
