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
$__start_mem = memory_get_usage();
$__start = _now();
  function contains($xs, $x) {
  global $valid_terms;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function join_with_comma($xs) {
  global $valid_terms;
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $s = $s . ', ';
}
  $s = $s . $xs[$i];
  $i = $i + 1;
};
  return $s;
};
  $valid_terms = ['approved_at_utc', 'approved_by', 'author_flair_background_color', 'author_flair_css_class', 'author_flair_richtext', 'author_flair_template_id', 'author_fullname', 'author_premium', 'can_mod_post', 'category', 'clicked', 'content_categories', 'created_utc', 'downs', 'edited', 'gilded', 'gildings', 'hidden', 'hide_score', 'is_created_from_ads_ui', 'is_meta', 'is_original_content', 'is_reddit_media_domain', 'is_video', 'link_flair_css_class', 'link_flair_richtext', 'link_flair_text', 'link_flair_text_color', 'media_embed', 'mod_reason_title', 'name', 'permalink', 'pwls', 'quarantine', 'saved', 'score', 'secure_media', 'secure_media_embed', 'selftext', 'subreddit', 'subreddit_name_prefixed', 'subreddit_type', 'thumbnail', 'title', 'top_awarded_type', 'total_awards_received', 'ups', 'upvote_ratio', 'url', 'user_reports'];
  function get_subreddit_data($subreddit, $limit, $age, $wanted_data) {
  global $valid_terms;
  $invalid = [];
  $i = 0;
  while ($i < count($wanted_data)) {
  $term = $wanted_data[$i];
  if (!contains($valid_terms, $term)) {
  $invalid = _append($invalid, $term);
}
  $i = $i + 1;
};
  if (count($invalid) > 0) {
  $msg = 'Invalid search term: ' . join_with_comma($invalid);
  _panic($msg);
}
  $resp = _fetch('tests/github/TheAlgorithms/Mochi/web_programming/reddit_sample.json');
  $result = [];
  $idx = 0;
  while ($idx < $limit) {
  $post = $resp['data']['children'][$idx]['data'];
  $post_map = [];
  if (count($wanted_data) == 0) {
  $post_map['title'] = $post['title'];
  $post_map['url'] = $post['url'];
  $post_map['selftext'] = $post['selftext'];
} else {
  $j = 0;
  while ($j < count($wanted_data)) {
  $field = $wanted_data[$j];
  if ($field == 'title') {
  $post_map['title'] = $post['title'];
} else {
  if ($field == 'url') {
  $post_map['url'] = $post['url'];
} else {
  if ($field == 'selftext') {
  $post_map['selftext'] = $post['selftext'];
};
};
}
  $j = $j + 1;
};
}
  $result[$idx] = $post_map;
  $idx = $idx + 1;
};
  return $result;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(get_subreddit_data('learnpython', 1, 'new', ['title', 'url', 'selftext']), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
