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
$MAX_LOCAL_PART_OCTETS = 64;
$MAX_DOMAIN_OCTETS = 255;
$ASCII_LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
$DIGITS = '0123456789';
$LOCAL_EXTRA = '.(!#$%&\'*+-/=?^_`{|}~)';
$DOMAIN_EXTRA = '.-';
function count_char($s, $target) {
  global $MAX_LOCAL_PART_OCTETS, $MAX_DOMAIN_OCTETS, $ASCII_LETTERS, $DIGITS, $LOCAL_EXTRA, $DOMAIN_EXTRA, $email_tests, $idx, $email;
  $cnt = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, _iadd($i, 1) - $i) == $target) {
  $cnt = _iadd($cnt, 1);
}
  $i = _iadd($i, 1);
};
  return $cnt;
}
function char_in($c, $allowed) {
  global $MAX_LOCAL_PART_OCTETS, $MAX_DOMAIN_OCTETS, $ASCII_LETTERS, $DIGITS, $LOCAL_EXTRA, $DOMAIN_EXTRA, $email_tests, $idx, $email;
  $i = 0;
  while ($i < strlen($allowed)) {
  if (substr($allowed, $i, _iadd($i, 1) - $i) == $c) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function starts_with_char($s, $c) {
  global $MAX_LOCAL_PART_OCTETS, $MAX_DOMAIN_OCTETS, $ASCII_LETTERS, $DIGITS, $LOCAL_EXTRA, $DOMAIN_EXTRA, $email_tests, $idx, $email;
  return strlen($s) > 0 && substr($s, 0, 1) == $c;
}
function ends_with_char($s, $c) {
  global $MAX_LOCAL_PART_OCTETS, $MAX_DOMAIN_OCTETS, $ASCII_LETTERS, $DIGITS, $LOCAL_EXTRA, $DOMAIN_EXTRA, $email_tests, $idx, $email;
  return strlen($s) > 0 && substr($s, _isub(strlen($s), 1), strlen($s) - _isub(strlen($s), 1)) == $c;
}
function contains_double_dot($s) {
  global $MAX_LOCAL_PART_OCTETS, $MAX_DOMAIN_OCTETS, $ASCII_LETTERS, $DIGITS, $LOCAL_EXTRA, $DOMAIN_EXTRA, $email_tests, $idx, $email;
  if (strlen($s) < 2) {
  return false;
}
  $i = 0;
  while ($i < _isub(strlen($s), 1)) {
  if (substr($s, $i, _iadd($i, 2) - $i) == '..') {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function is_valid_email_address($email) {
  global $MAX_LOCAL_PART_OCTETS, $MAX_DOMAIN_OCTETS, $ASCII_LETTERS, $DIGITS, $LOCAL_EXTRA, $DOMAIN_EXTRA, $email_tests, $idx;
  if (count_char($email, '@') != 1) {
  return false;
}
  $at_idx = 0;
  $i = 0;
  while ($i < strlen($email)) {
  if (substr($email, $i, _iadd($i, 1) - $i) == '@') {
  $at_idx = $i;
  break;
}
  $i = _iadd($i, 1);
};
  $local_part = substr($email, 0, $at_idx);
  $domain = substr($email, _iadd($at_idx, 1), strlen($email) - _iadd($at_idx, 1));
  if (strlen($local_part) > $MAX_LOCAL_PART_OCTETS || strlen($domain) > $MAX_DOMAIN_OCTETS) {
  return false;
}
  $i = 0;
  while ($i < strlen($local_part)) {
  $ch = substr($local_part, $i, _iadd($i, 1) - $i);
  if (!char_in($ch, $ASCII_LETTERS . $DIGITS . $LOCAL_EXTRA)) {
  return false;
}
  $i = _iadd($i, 1);
};
  if (starts_with_char($local_part, '.') || ends_with_char($local_part, '.') || contains_double_dot($local_part)) {
  return false;
}
  $i = 0;
  while ($i < strlen($domain)) {
  $ch = substr($domain, $i, _iadd($i, 1) - $i);
  if (!char_in($ch, $ASCII_LETTERS . $DIGITS . $DOMAIN_EXTRA)) {
  return false;
}
  $i = _iadd($i, 1);
};
  if (starts_with_char($domain, '-') || ends_with_char($domain, '.')) {
  return false;
}
  if (starts_with_char($domain, '.') || ends_with_char($domain, '.') || contains_double_dot($domain)) {
  return false;
}
  return true;
}
$email_tests = ['simple@example.com', 'very.common@example.com', 'disposable.style.email.with+symbol@example.com', 'other-email-with-hyphen@and.subdomains.example.com', 'fully-qualified-domain@example.com', 'user.name+tag+sorting@example.com', 'x@example.com', 'example-indeed@strange-example.com', 'test/test@test.com', '123456789012345678901234567890123456789012345678901234567890123@example.com', 'admin@mailserver1', 'example@s.example', 'Abc.example.com', 'A@b@c@example.com', 'abc@example..com', 'a(c)d,e:f;g<h>i[j\\k]l@example.com', '12345678901234567890123456789012345678901234567890123456789012345@example.com', 'i.like.underscores@but_its_not_allowed_in_this_part', ''];
$idx = 0;
while ($idx < count($email_tests)) {
  $email = $email_tests[$idx];
  echo rtrim(_str(is_valid_email_address($email))), PHP_EOL;
  $idx = _iadd($idx, 1);
}
