<?php
function ladders($begin, $end, $words) {
    $wordSet = array_fill_keys($words, true);
    if (!isset($wordSet[$end])) return [];
    $parents = [];
    $level = [$begin => true];
    $visited = [$begin => true];
    $found = false;
    while (!empty($level) && !$found) {
        $nextLevel = [];
        foreach (array_keys($level) as $word) {
            $chars = str_split($word);
            for ($i = 0; $i < count($chars); $i++) {
                $orig = $chars[$i];
                for ($c = ord('a'); $c <= ord('z'); $c++) {
                    $ch = chr($c);
                    if ($ch === $orig) continue;
                    $chars[$i] = $ch;
                    $nw = implode('', $chars);
                    if (!isset($wordSet[$nw]) || isset($visited[$nw])) continue;
                    $nextLevel[$nw] = true;
                    if (!isset($parents[$nw])) $parents[$nw] = [];
                    $parents[$nw][] = $word;
                    if ($nw === $end) $found = true;
                }
                $chars[$i] = $orig;
            }
        }
        foreach ($nextLevel as $w => $_) $visited[$w] = true;
        $level = $nextLevel;
    }
    if (!$found) return [];
    $out = [];
    $path = [$end];
    $backtrack = function($word) use (&$backtrack, &$out, &$path, $begin, $parents) {
        if ($word === $begin) {
            $out[] = array_reverse($path);
            return;
        }
        $plist = $parents[$word] ?? [];
        sort($plist);
        foreach ($plist as $p) {
            $path[] = $p;
            $backtrack($p);
            array_pop($path);
        }
    };
    $backtrack($end);
    usort($out, function($a, $b) { return strcmp(implode('->', $a), implode('->', $b)); });
    return $out;
}
function fmt($paths) {
    $lines = [strval(count($paths))];
    foreach ($paths as $p) $lines[] = implode('->', $p);
    return implode("\n", $lines);
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$tc = intval($lines[0]); $idx = 1; $out = [];
for ($t = 0; $t < $tc; $t++) {
    $begin = $lines[$idx++];
    $end = $lines[$idx++];
    $n = intval($lines[$idx++]);
    $words = array_slice($lines, $idx, $n); $idx += $n;
    $out[] = fmt(ladders($begin, $end, $words));
}
echo implode("\n\n", $out);
?>
