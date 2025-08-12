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
  function http_post($secret, $client) {
  $success = $secret == 'secretKey' && $client == 'clientKey';
  return ['success' => $success];
};
  function authenticate($username, $password) {
  return $username == 'user' && $password == 'pass';
};
  function login($_user) {
};
  function render($page) {
  return 'render:' . $page;
};
  function redirect($url) {
  return 'redirect:' . $url;
};
  function login_using_recaptcha($request) {
  $secret_key = 'secretKey';
  if ($request['method'] != 'POST') {
  return render('login.html');
}
  $username = $request['post']['username'];
  $password = $request['post']['password'];
  $client_key = $request['post']['g-recaptcha-response'];
  $response = http_post($secret_key, $client_key);
  if ($response['success']) {
  if (authenticate($username, $password)) {
  login($username);
  return redirect('/your-webpage');
};
}
  return render('login.html');
};
  $get_request = ['method' => 'GET', 'post' => []];
  echo rtrim(login_using_recaptcha($get_request)), PHP_EOL;
  $ok_request = ['method' => 'POST', 'post' => ['username' => 'user', 'password' => 'pass', 'g-recaptcha-response' => 'clientKey']];
  echo rtrim(login_using_recaptcha($ok_request)), PHP_EOL;
  $bad_request = ['method' => 'POST', 'post' => ['username' => 'user', 'password' => 'wrong', 'g-recaptcha-response' => 'clientKey']];
  echo rtrim(login_using_recaptcha($bad_request)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
