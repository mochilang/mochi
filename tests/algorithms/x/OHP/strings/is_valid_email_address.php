<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
$__start_mem = memory_get_usage();
$__start = _now();
  $MAX_LOCAL_PART_OCTETS = 64;
  $MAX_DOMAIN_OCTETS = 255;
  $ASCII_LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $DIGITS = '0123456789';
  $LOCAL_EXTRA = '.(!#$%&\'*+-/=?^_`{|}~)';
  $DOMAIN_EXTRA = '.-';
  function count_char($s, $target) {
  global $ASCII_LETTERS, $DIGITS, $DOMAIN_EXTRA, $LOCAL_EXTRA, $MAX_DOMAIN_OCTETS, $MAX_LOCAL_PART_OCTETS, $email, $email_tests, $idx;
  $cnt = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $target) {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
};
  function char_in($c, $allowed) {
  global $ASCII_LETTERS, $DIGITS, $DOMAIN_EXTRA, $LOCAL_EXTRA, $MAX_DOMAIN_OCTETS, $MAX_LOCAL_PART_OCTETS, $email, $email_tests, $idx;
  $i = 0;
  while ($i < strlen($allowed)) {
  if (substr($allowed, $i, $i + 1 - $i) == $c) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function starts_with_char($s, $c) {
  global $ASCII_LETTERS, $DIGITS, $DOMAIN_EXTRA, $LOCAL_EXTRA, $MAX_DOMAIN_OCTETS, $MAX_LOCAL_PART_OCTETS, $email, $email_tests, $idx;
  return strlen($s) > 0 && substr($s, 0, 1) == $c;
};
  function ends_with_char($s, $c) {
  global $ASCII_LETTERS, $DIGITS, $DOMAIN_EXTRA, $LOCAL_EXTRA, $MAX_DOMAIN_OCTETS, $MAX_LOCAL_PART_OCTETS, $email, $email_tests, $idx;
  return strlen($s) > 0 && substr($s, strlen($s) - 1, strlen($s) - (strlen($s) - 1)) == $c;
};
  function contains_double_dot($s) {
  global $ASCII_LETTERS, $DIGITS, $DOMAIN_EXTRA, $LOCAL_EXTRA, $MAX_DOMAIN_OCTETS, $MAX_LOCAL_PART_OCTETS, $email, $email_tests, $idx;
  if (strlen($s) < 2) {
  return false;
}
  $i = 0;
  while ($i < strlen($s) - 1) {
  if (substr($s, $i, $i + 2 - $i) == '..') {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function is_valid_email_address($email) {
  global $ASCII_LETTERS, $DIGITS, $DOMAIN_EXTRA, $LOCAL_EXTRA, $MAX_DOMAIN_OCTETS, $MAX_LOCAL_PART_OCTETS, $email_tests, $idx;
  if (count_char($email, '@') != 1) {
  return false;
}
  $at_idx = 0;
  $i = 0;
  while ($i < strlen($email)) {
  if (substr($email, $i, $i + 1 - $i) == '@') {
  $at_idx = $i;
  break;
}
  $i = $i + 1;
};
  $local_part = substr($email, 0, $at_idx);
  $domain = substr($email, $at_idx + 1, strlen($email) - ($at_idx + 1));
  if (strlen($local_part) > $MAX_LOCAL_PART_OCTETS || strlen($domain) > $MAX_DOMAIN_OCTETS) {
  return false;
}
  $i = 0;
  while ($i < strlen($local_part)) {
  $ch = substr($local_part, $i, $i + 1 - $i);
  if (!char_in($ch, $ASCII_LETTERS . $DIGITS . $LOCAL_EXTRA)) {
  return false;
}
  $i = $i + 1;
};
  if (starts_with_char($local_part, '.') || ends_with_char($local_part, '.') || contains_double_dot($local_part)) {
  return false;
}
  $i = 0;
  while ($i < strlen($domain)) {
  $ch = substr($domain, $i, $i + 1 - $i);
  if (!char_in($ch, $ASCII_LETTERS . $DIGITS . $DOMAIN_EXTRA)) {
  return false;
}
  $i = $i + 1;
};
  if (starts_with_char($domain, '-') || ends_with_char($domain, '.')) {
  return false;
}
  if (starts_with_char($domain, '.') || ends_with_char($domain, '.') || contains_double_dot($domain)) {
  return false;
}
  return true;
};
  $email_tests = ['simple@example.com', 'very.common@example.com', 'disposable.style.email.with+symbol@example.com', 'other-email-with-hyphen@and.subdomains.example.com', 'fully-qualified-domain@example.com', 'user.name+tag+sorting@example.com', 'x@example.com', 'example-indeed@strange-example.com', 'test/test@test.com', '123456789012345678901234567890123456789012345678901234567890123@example.com', 'admin@mailserver1', 'example@s.example', 'Abc.example.com', 'A@b@c@example.com', 'abc@example..com', 'a(c)d,e:f;g<h>i[j\\k]l@example.com', '12345678901234567890123456789012345678901234567890123456789012345@example.com', 'i.like.underscores@but_its_not_allowed_in_this_part', ''];
  $idx = 0;
  while ($idx < count($email_tests)) {
  $email = $email_tests[$idx];
  echo rtrim(_str(is_valid_email_address($email))), PHP_EOL;
  $idx = $idx + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
