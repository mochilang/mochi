<?php
ini_set('memory_limit', '-1');
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
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
$BASE_URL = 'https://api.carbonintensity.org.uk/intensity';
function fetch_last_half_hour() {
  global $BASE_URL;
  $resp = _fetch($BASE_URL, ['timeout' => 10.0]);
  $entry = $resp['data'][0];
  return $entry['intensity']['actual'];
}
function fetch_from_to($start, $end) {
  global $BASE_URL;
  $url = $BASE_URL . '/' . $start . '/' . $end;
  $resp = _fetch($url, ['timeout' => 10.0]);
  return $resp['data'];
}
function main() {
  global $BASE_URL;
  $entries = fetch_from_to('2020-10-01', '2020-10-03');
  $i = 0;
  while ($i < count($entries)) {
  $e = $entries[$i];
  echo rtrim('from') . " " . rtrim(json_encode($e['from'], 1344)) . " " . rtrim('to') . " " . rtrim(json_encode($e['to'], 1344)) . " " . rtrim(':') . " " . rtrim(json_encode($e['intensity']['actual'], 1344)), PHP_EOL;
  $i = _iadd($i, 1);
};
  $last = fetch_last_half_hour();
  echo rtrim('fetch_last_half_hour() =') . " " . rtrim(json_encode($last, 1344)), PHP_EOL;
}
main();
