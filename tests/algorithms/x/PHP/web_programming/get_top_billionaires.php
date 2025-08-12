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
$LIMIT = 10;
$TODAY_MS = 1705017600000.0;
$API_URL = 'https://www.forbes.com/forbesapi/person/rtb/0/position/true.json?fields=personName,gender,source,countryOfCitizenship,birthDate,finalWorth&limit=' . _str($LIMIT);
function round1($value) {
  if ($value >= 0.0) {
  $scaled = intval(($value * 10.0 + 0.5));
  return (floatval($scaled)) / 10.0;
}
  $scaled = intval(($value * 10.0 - 0.5));
  return (floatval($scaled)) / 10.0;
}
function years_old($birth_ms, $today_ms) {
  $ms_per_year = 31557600000.0;
  return intval((($today_ms - $birth_ms) / $ms_per_year));
}
function get_forbes_real_time_billionaires() {
  global $API_URL, $TODAY_MS;
  $response = _fetch($API_URL);
  $out = [];
  foreach ($response['personList']['personsLists'] as $person) {
  $worth_billion = round1($person['finalWorth'] / 1000.0);
  $age_years = years_old($person['birthDate'], $TODAY_MS);
  $entry = ['Name' => $person['personName'], 'Source' => $person['source'], 'Country' => $person['countryOfCitizenship'], 'Gender' => $person['gender'], 'Worth ($)' => _str($worth_billion) . ' Billion', 'Age' => _str($age_years)];
  $out = _append($out, $entry);
};
  return $out;
}
function display_billionaires($list) {
  foreach ($list as $b) {
  echo rtrim($b['Name'] . ' | ' . $b['Source'] . ' | ' . $b['Country'] . ' | ' . $b['Gender'] . ' | ' . $b['Worth ($)'] . ' | ' . $b['Age']), PHP_EOL;
};
}
display_billionaires(get_forbes_real_time_billionaires());
