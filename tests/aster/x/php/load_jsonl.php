<?php
$people = (function() {
    $rows = [];
    foreach (file("/workspace/mochi/tests/interpreter/valid/people.jsonl", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
        $line = trim($line);
        if ($line === "''") {
        }
        $rows[] = json_decode($line);
    }
    return $rows;
}
)();
$adults = [];
foreach ($people as $p) {
    if ($p["age"] >= 18) {
        $adults[] = ["name" => $p["name"], "email" => $p["email"]];
    }
}
foreach ($adults as $a) {
    echo  . (is_float($a["email"]) ? json_encode($a["email"], 1344) : $a["email"]), PHP_EOL;
}
