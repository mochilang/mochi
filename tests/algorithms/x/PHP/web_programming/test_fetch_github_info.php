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
$__start_mem = memory_get_usage();
$__start = _now();
  $AUTHENTICATED_USER_ENDPOINT = 'https://api.github.com/user';
  function get_header($hs, $key) {
  global $AUTHENTICATED_USER_ENDPOINT;
  $i = 0;
  while ($i < count($hs)) {
  $pair = $hs[$i];
  if ($pair[0] == $key) {
  return $pair[1];
}
  $i = $i + 1;
};
  return '';
};
  function mock_response($url, $headers) {
  global $AUTHENTICATED_USER_ENDPOINT;
  if ($url != $AUTHENTICATED_USER_ENDPOINT) {
  _panic('wrong url');
}
  $auth = get_header($headers, 'Authorization');
  if (strlen($auth) == 0) {
  _panic('missing Authorization');
}
  if (substr($auth, 0, 6) != 'token ') {
  _panic('bad token prefix');
}
  $accept = get_header($headers, 'Accept');
  if (strlen($accept) == 0) {
  _panic('missing Accept');
}
  return ['login' => 'test', 'id' => 1];
};
  function fetch_github_info($auth_token) {
  global $AUTHENTICATED_USER_ENDPOINT;
  $headers = [['Authorization', 'token ' . $auth_token], ['Accept', 'application/vnd.github.v3+json']];
  return mock_response($AUTHENTICATED_USER_ENDPOINT, $headers);
};
  function test_fetch_github_info() {
  global $AUTHENTICATED_USER_ENDPOINT;
  $result = fetch_github_info('token');
  if ($result['login'] != 'test') {
  _panic('login mismatch');
}
  if ($result['id'] != 1) {
  _panic('id mismatch');
}
  echo rtrim(json_encode($result['login'], 1344)), PHP_EOL;
  echo rtrim(_str($result['id'])), PHP_EOL;
};
  function main() {
  global $AUTHENTICATED_USER_ENDPOINT;
  test_fetch_github_info();
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
