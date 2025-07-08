<?php
class Todo {
    public $title;
    public function __construct($fields = []) {
        $this->title = $fields['title'] ?? null;
    }
}

$todo = new Todo(['title' => 'hi']);
var_dump($todo->title);
?>
