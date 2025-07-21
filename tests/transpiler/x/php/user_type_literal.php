<?php
$book = ["title" => "Go", "author" => ["name" => "Bob", "age" => 42]];
echo (is_float($book["author"]["name"]) ? json_encode($book["author"]["name"], 1344) : $book["author"]["name"]), PHP_EOL;
