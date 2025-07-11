<?php
class Person {
    public $name;
    public $age;
    public function __construct($fields = []) {
        $this->name = $fields['name'] ?? null;
        $this->age = $fields['age'] ?? null;
    }
}
class Book {
    public $title;
    public $author;
    public function __construct($fields = []) {
        $this->title = $fields['title'] ?? null;
        $this->author = $fields['author'] ?? null;
    }
}
$book = new Book([
    'title' => "Go",
    'author' => new Person(['name' => "Bob", 'age' => 42])
]);
var_dump($book->author->name);
?>
