<?php
function isScramble($s1, $s2) {
    $memo = [];
    $dfs = function($i1, $i2, $len) use ($s1, $s2, &$memo, &$dfs) {
        $key = $i1 . "," . $i2 . "," . $len;
        if (array_key_exists($key, $memo)) return $memo[$key];
        $a = substr($s1, $i1, $len);
        $b = substr($s2, $i2, $len);
        if ($a === $b) return $memo[$key] = true;
        $cnt = array_fill(0, 26, 0);
        for ($i = 0; $i < $len; $i++) {
            $cnt[ord($a[$i]) - 97]++;
            $cnt[ord($b[$i]) - 97]--;
        }
        foreach ($cnt as $v) if ($v != 0) return $memo[$key] = false;
        for ($k = 1; $k < $len; $k++) {
            if (($dfs($i1, $i2, $k) && $dfs($i1 + $k, $i2 + $k, $len - $k)) ||
                ($dfs($i1, $i2 + $len - $k, $k) && $dfs($i1 + $k, $i2, $len - $k))) return $memo[$key] = true;
        }
        return $memo[$key] = false;
    };
    return $dfs(0, 0, strlen($s1));
}

$lines = preg_split("/\\r?\\n/", stream_get_contents(STDIN));
if ($lines && trim($lines[0]) !== "") {
    $t = intval(trim($lines[0]));
    $out = [];
    for ($i = 0; $i < $t; $i++) $out[] = isScramble($lines[1 + 2 * $i], $lines[2 + 2 * $i]) ? "true" : "false";
    echo implode("\n", $out);
}
?>
