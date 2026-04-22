<?php
function findSet(int $x, array &$parent): int {
    while ($parent[$x] !== $x) {
        $parent[$x] = $parent[$parent[$x]];
        $x = $parent[$x];
    }
    return $x;
}

function unionSet(int $a, int $b, array &$parent, array &$rank): bool {
    $ra = findSet($a, $parent);
    $rb = findSet($b, $parent);
    if ($ra === $rb) return false;
    if ($rank[$ra] < $rank[$rb]) [$ra, $rb] = [$rb, $ra];
    $parent[$rb] = $ra;
    if ($rank[$ra] === $rank[$rb]) $rank[$ra]++;
    return true;
}

function solve(int $m, int $n, array $positions): array {
    $parent = [];
    $rank = [];
    $count = 0;
    $ans = [];
    foreach ($positions as [$r, $c]) {
        $idx = $r * $n + $c;
        if (isset($parent[$idx])) {
            $ans[] = $count;
            continue;
        }
        $parent[$idx] = $idx;
        $rank[$idx] = 0;
        $count++;
        foreach ([[1,0],[-1,0],[0,1],[0,-1]] as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;
            if ($nr >= 0 && $nr < $m && $nc >= 0 && $nc < $n) {
                $nei = $nr * $n + $nc;
                if (isset($parent[$nei]) && unionSet($idx, $nei, $parent, $rank)) $count--;
            }
        }
        $ans[] = $count;
    }
    return $ans;
}

$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$data || $data[0] === '') exit(0);
$idx = 0;
$t = intval($data[$idx++]);
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $m = intval($data[$idx++]);
    $n = intval($data[$idx++]);
    $k = intval($data[$idx++]);
    $positions = [];
    for ($i = 0; $i < $k; $i++) $positions[] = [intval($data[$idx++]), intval($data[$idx++])];
    $blocks[] = '[' . implode(',', solve($m, $n, $positions)) . ']';
}
echo implode("\n\n", $blocks);
