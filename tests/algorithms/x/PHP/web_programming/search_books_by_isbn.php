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
    if ($x === null) { return 0; }
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function count_char($s, $ch) {
  $cnt = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
};
  function strip($s) {
  $start = 0;
  $end = strlen($s);
  while ($start < $end && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  while ($end > $start && substr($s, $end - 1, $end - ($end - 1)) == ' ') {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function trim_slashes($s) {
  $start = 0;
  $end = strlen($s);
  while ($start < $end && substr($s, $start, $start + 1 - $start) == '/') {
  $start = $start + 1;
};
  while ($end > $start && substr($s, $end - 1, $end - ($end - 1)) == '/') {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function normalize_olid($olid) {
  $stripped = strip($olid);
  $cleaned = trim_slashes($stripped);
  if (count_char($cleaned, '/') != 1) {
  _panic($olid . ' is not a valid Open Library olid');
}
  return $cleaned;
};
  function get_book_data($olid) {
  $norm = normalize_olid($olid);
  $url = 'https://openlibrary.org/' . $norm . '.json';
  $data = _fetch($url);
  return $data;
};
  function get_author_data($olid) {
  $norm = normalize_olid($olid);
  $url = 'https://openlibrary.org/' . $norm . '.json';
  $data = _fetch($url);
  return $data;
};
  function summarize_book($book) {
  $names = [];
  $i = 0;
  while ($i < _len($book['authors'])) {
  $ref = $book['authors'][$i];
  $auth = get_author_data($ref['key']);
  $names = _append($names, $auth['name']);
  $i = $i + 1;
};
  return ['title' => $book['title'], 'publish_date' => $book['publish_date'], 'authors' => mochi_join($names, ', '), 'number_of_pages' => $book['number_of_pages'], 'isbn_10' => mochi_join($book['isbn_10'], ', '), 'isbn_13' => mochi_join($book['isbn_13'], ', ')];
};
  function main() {
  $book = get_book_data('isbn/0140328726');
  $summary = summarize_book($book);
  echo rtrim('Title: ' . $summary['title']), PHP_EOL;
  echo rtrim('Publish date: ' . $summary['publish_date']), PHP_EOL;
  echo rtrim('Authors: ' . $summary['authors']), PHP_EOL;
  echo rtrim('Number of pages: ' . _str($summary['number_of_pages'])), PHP_EOL;
  echo rtrim('ISBN (10): ' . $summary['isbn_10']), PHP_EOL;
  echo rtrim('ISBN (13): ' . $summary['isbn_13']), PHP_EOL;
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
