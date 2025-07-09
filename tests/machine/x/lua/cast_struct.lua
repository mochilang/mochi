Todo = {}
Todo.__index = Todo
function Todo.new(o)
  o = o or {}
  setmetatable(o, Todo)
  return o
end

todo = Todo.new({["title"]="hi"})
print(todo.title)
