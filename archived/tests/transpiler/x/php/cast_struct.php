<?php
$todo = ["title" => "hi"];
echo (is_float($todo["title"]) ? json_encode($todo["title"], 1344) : $todo["title"]), PHP_EOL;
