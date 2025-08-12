<?php
ini_set('memory_limit', '-1');
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
function get_apod_data($api_key) {
  $data = _fetch('https://api.nasa.gov/planetary/apod?api_key=DEMO_KEY');
  return $data;
}
function save_apod($api_key) {
  $apod = get_apod_data($api_key);
  return $apod;
}
function get_archive_data($query) {
  $data = _fetch('https://images-api.nasa.gov/search?q=apollo%202011');
  return $data;
}
function main() {
  $apod = save_apod('DEMO_KEY');
  echo rtrim(json_encode($apod['title'], 1344)), PHP_EOL;
  $archive = get_archive_data('apollo 2011');
  $items = $archive['collection']['items'];
  $first_item = $items[0];
  $first_data = $first_item['data'][0];
  echo rtrim(json_encode($first_data['description'], 1344)), PHP_EOL;
}
main();
