<?php
ini_set('memory_limit', '-1');
function is_leap($year) {
  if (($year % 4 == 0 && $year % 100 != 0) || ($year % 400 == 0)) {
  return true;
}
  return false;
}
function count_sundays() {
  $days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  $day = 6;
  $month = 1;
  $year = 1901;
  $sundays = 0;
  while ($year < 2001) {
  $day = $day + 7;
  if (is_leap($year)) {
  if ($day > $days_per_month[$month - 1] && $month != 2) {
  $month = $month + 1;
  $day = $day - $days_per_month[$month - 2];
} else {
  if ($day > 29 && $month == 2) {
  $month = $month + 1;
  $day = $day - 29;
};
};
} else {
  if ($day > $days_per_month[$month - 1]) {
  $month = $month + 1;
  $day = $day - $days_per_month[$month - 2];
};
}
  if ($month > 12) {
  $year = $year + 1;
  $month = 1;
}
  if ($year < 2001 && $day == 1) {
  $sundays = $sundays + 1;
}
};
  return $sundays;
}
echo rtrim(json_encode(count_sundays(), 1344)), PHP_EOL;
