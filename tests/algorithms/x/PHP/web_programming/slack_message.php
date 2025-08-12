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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function _fetch($url, $opts = null) {
    $method = 'GET';
    $headers = [];
    $body = null;
    $query = null;
    $timeout = 0;
    if ($opts !== null) {
        if (isset($opts['method'])) $method = strtoupper($opts['method']);
        if (isset($opts['headers'])) {
            foreach ($opts['headers'] as $k => $v) { $headers[] = "$k: $v"; }
        }
        if (isset($opts['body'])) $body = json_encode($opts['body']);
        if (isset($opts['query'])) $query = http_build_query($opts['query']);
        if (isset($opts['timeout'])) $timeout = intval($opts['timeout']);
    }
    if ($query !== null) {
        $url .= (strpos($url, '?') !== false ? '&' : '?') . $query;
    }
    $context = ['http' => ['method' => $method, 'header' => implode("\r\n", $headers)]];
    if ($body !== null) $context['http']['content'] = $body;
    if ($timeout > 0) $context['http']['timeout'] = $timeout / 1000.0;
    $ctx = stream_context_create($context);
    $data = file_get_contents($url, false, $ctx);
    return json_decode($data, true);
}
function _fetch_str($url, $opts = null) {
    $method = 'GET';
    $headers = [];
    $body = null;
    $query = null;
    $timeout = 0;
    if ($opts !== null) {
        if (isset($opts['method'])) $method = strtoupper($opts['method']);
        if (isset($opts['headers'])) {
            foreach ($opts['headers'] as $k => $v) { $headers[] = "$k: $v"; }
        }
        if (isset($opts['body'])) $body = json_encode($opts['body']);
        if (isset($opts['query'])) $query = http_build_query($opts['query']);
        if (isset($opts['timeout'])) $timeout = intval($opts['timeout']);
    }
    if ($query !== null) {
        $url .= (strpos($url, '?') !== false ? '&' : '?') . $query;
    }
    $context = ['http' => ['method' => $method, 'header' => implode("\r\n", $headers)]];
    if ($body !== null) $context['http']['content'] = $body;
    if ($timeout > 0) $context['http']['timeout'] = $timeout / 1000.0;
    $ctx = stream_context_create($context);
    return file_get_contents($url, false, $ctx);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function send_slack_message($message_body, $slack_url) {
  global $url;
  $headers = ['Content-Type' => 'application/json'];
  $response = _fetch_str(strval($slack_url), ['method' => 'POST', 'headers' => $headers, 'body' => ['text' => $message_body]]);
  if ($response != 'ok') {
  _panic('Request to slack returned an error: ' . $response);
}
  return $response;
};
  $url = 'https://httpbin.org/post';
  send_slack_message('Hello from Mochi', $url);
  echo rtrim('Message sent'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
