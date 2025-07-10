function __print(...)
    local args = {...}
    local parts = {}
    for i,a in ipairs(args) do
        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end
    end
    print(table.concat(parts, ' '))
end
Todo = {}
Todo.__index = Todo
function Todo.new(o)
  o = o or {}
  setmetatable(o, Todo)
  return o
end

todo = Todo.new({["title"]="hi"})
__print(todo.title)
