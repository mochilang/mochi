<?php
ini_set('memory_limit', '-1');
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
function get_hackernews_story($story_id) {
  $url = 'https://hacker-news.firebaseio.com/v0/item/' . _str($story_id) . '.json?print=pretty';
  $story = (_fetch($url));
  if ($story['url'] == '') {
  $story['url'] = 'https://news.ycombinator.com/item?id=' . _str($story_id);
}
  return $story;
}
function hackernews_top_stories($max_stories) {
  $url = 'https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty';
  $ids = (_fetch($url));
  $ids = array_slice($ids, 0, $max_stories);
  $stories = [];
  $i = 0;
  while ($i < count($ids)) {
  $stories = _append($stories, get_hackernews_story($ids[$i]));
  $i = $i + 1;
};
  return $stories;
}
function hackernews_top_stories_as_markdown($max_stories) {
  $stories = hackernews_top_stories($max_stories);
  $output = '';
  $i = 0;
  while ($i < count($stories)) {
  $s = $stories[$i];
  $line = '* [' . $s['title'] . '](' . $s['url'] . ')';
  if ($i == 0) {
  $output = $line;
} else {
  $output = $output . '
' . $line;
}
  $i = $i + 1;
};
  return $output;
}
echo rtrim(hackernews_top_stories_as_markdown(5)), PHP_EOL;
