class TreeNode
  attr_accessor :val, :left, :right

  def initialize(val)
    @val = val
    @left = nil
    @right = nil
  end
end

def serialize(root)
  return "[]" if root.nil?

  out = []
  q = [root]
  until q.empty?
    node = q.shift
    if node.nil?
      out << "null"
    else
      out << node.val.to_s
      q << node.left << node.right
    end
  end
  out.pop while out[-1] == "null"
  "[#{out.join(',')}]"
end

def deserialize(data)
  return nil if data == "[]"

  vals = data[1...-1].split(",")
  root = TreeNode.new(vals[0].to_i)
  q = [root]
  i = 1
  until q.empty? || i >= vals.length
    node = q.shift
    if i < vals.length && vals[i] != "null"
      node.left = TreeNode.new(vals[i].to_i)
      q << node.left
    end
    i += 1
    if i < vals.length && vals[i] != "null"
      node.right = TreeNode.new(vals[i].to_i)
      q << node.right
    end
    i += 1
  end
  root
end

lines = STDIN.read.lines.map(&:strip).reject(&:empty?)
exit if lines.empty?

t = lines[0].to_i
out = []
t.times do |tc|
  out << serialize(deserialize(lines[tc + 1]))
end
print out.join("\n\n")
