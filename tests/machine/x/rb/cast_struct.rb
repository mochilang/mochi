Todo = Struct.new(:title, keyword_init: true)

$todo = Todo.new(**({"title" => "hi"}.to_h.transform_keys(&:to_sym)))
puts($todo.title)
