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
function index_of($s, $sub) {
  $i = 0;
  while ($i <= strlen($s) - strlen($sub)) {
  if (substr($s, $i, $i + strlen($sub) - $i) == $sub) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function parse_int($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $value = $value * 10 + ((ctype_digit($s[$i]) ? intval($s[$i]) : ord($s[$i])));
  $i = $i + 1;
};
  return $value;
}
function extract_string($text, $key) {
  $pattern = '"' . $key . '":"';
  $start = index_of($text, $pattern) + strlen($pattern);
  $end = $start;
  while ($end < strlen($text) && substr($text, $end, $end + 1 - $end) != '"') {
  $end = $end + 1;
};
  return substr($text, $start, $end - $start);
}
function extract_int($text, $key) {
  $pattern = '"' . $key . '":{"count":';
  $start = index_of($text, $pattern) + strlen($pattern);
  $end = $start;
  while ($end < strlen($text)) {
  $ch = substr($text, $end, $end + 1 - $end);
  if ($ch < '0' || $ch > '9') {
  break;
}
  $end = $end + 1;
};
  $digits = substr($text, $start, $end - $start);
  $num = parse_int($digits);
  return $num;
}
function extract_bool($text, $key) {
  $pattern = '"' . $key . '":';
  $start = index_of($text, $pattern) + strlen($pattern);
  $val = substr($text, $start, $start + 5 - $start);
  $first = substr($val, 0, 0 + 1);
  if ($first == 't') {
  return true;
}
  return false;
}
function extract_user_profile($script) {
  return ['username' => extract_string($script, 'username'), 'full_name' => extract_string($script, 'full_name'), 'biography' => extract_string($script, 'biography'), 'business_email' => extract_string($script, 'business_email'), 'external_url' => extract_string($script, 'external_url'), 'edge_followed_by' => ['count' => extract_int($script, 'edge_followed_by')], 'edge_follow' => ['count' => extract_int($script, 'edge_follow')], 'edge_owner_to_timeline_media' => ['count' => extract_int($script, 'edge_owner_to_timeline_media')], 'profile_pic_url_hd' => extract_string($script, 'profile_pic_url_hd'), 'is_verified' => extract_bool($script, 'is_verified'), 'is_private' => extract_bool($script, 'is_private')];
}
$sample_script = '{"entry_data":{"ProfilePage":[{"graphql":{"user":{"username":"github","full_name":"GitHub","biography":"Built for developers.","business_email":"support@github.com","external_url":"https://github.com/readme","edge_followed_by":{"count":120000},"edge_follow":{"count":16},"edge_owner_to_timeline_media":{"count":150},"profile_pic_url_hd":"https://instagram.com/pic.jpg","is_verified":true,"is_private":false}}}]}}';
$user = extract_user_profile($sample_script);
echo rtrim($user['full_name'] . ' (' . $user['username'] . ') is ' . $user['biography']), PHP_EOL;
echo rtrim('number_of_posts = ' . _str($user['edge_owner_to_timeline_media']['count'])), PHP_EOL;
echo rtrim('number_of_followers = ' . _str($user['edge_followed_by']['count'])), PHP_EOL;
echo rtrim('number_of_followings = ' . _str($user['edge_follow']['count'])), PHP_EOL;
echo rtrim('email = ' . $user['business_email']), PHP_EOL;
echo rtrim('website = ' . $user['external_url']), PHP_EOL;
echo rtrim('profile_picture_url = ' . $user['profile_pic_url_hd']), PHP_EOL;
echo rtrim('is_verified = ' . _str($user['is_verified'])), PHP_EOL;
echo rtrim('is_private = ' . _str($user['is_private'])), PHP_EOL;
