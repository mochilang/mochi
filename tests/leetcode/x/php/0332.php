<?php
function visit($a, &$graph, &$route) { while (isset($graph[$a]) && count($graph[$a]) > 0) { $next = array_pop($graph[$a]); visit($next, $graph, $route); } $route[] = $a; }
function findItinerary($tickets) { $graph = []; foreach ($tickets as $t) $graph[$t[0]][] = $t[1]; foreach ($graph as &$v) rsort($v, SORT_STRING); unset($v); $route = []; visit("JFK", $graph, $route); return array_reverse($route); }
function fmtRoute($r) { return "[" . implode(",", array_map(fn($s) => "\"$s\"", $r)) . "]"; }
$data = preg_split('/\s+/', trim(stream_get_contents(STDIN))); if (count($data) && $data[0] !== '') { $idx = 0; $t = intval($data[$idx++]); $out = []; for ($tc = 0; $tc < $t; $tc++) { $m = intval($data[$idx++]); $tickets = []; for ($i = 0; $i < $m; $i++) $tickets[] = [$data[$idx++], $data[$idx++]]; $out[] = fmtRoute(findItinerary($tickets)); } echo implode("\n\n", $out); }
?>
