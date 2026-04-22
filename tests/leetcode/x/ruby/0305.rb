def find_set(x, parent)
  while parent[x] != x
    parent[x] = parent[parent[x]]
    x = parent[x]
  end
  x
end

def union_set(a, b, parent, rank)
  ra = find_set(a, parent)
  rb = find_set(b, parent)
  return false if ra == rb

  ra, rb = rb, ra if rank[ra] < rank[rb]
  parent[rb] = ra
  rank[ra] += 1 if rank[ra] == rank[rb]
  true
end

def solve(m, n, positions)
  parent = {}
  rank = {}
  count = 0
  ans = []
  positions.each do |r, c|
    idx = r * n + c
    if parent.key?(idx)
      ans << count
      next
    end
    parent[idx] = idx
    rank[idx] = 0
    count += 1
    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dr, dc|
      nr = r + dr
      nc = c + dc
      next unless nr.between?(0, m - 1) && nc.between?(0, n - 1)

      nei = nr * n + nc
      count -= 1 if parent.key?(nei) && union_set(idx, nei, parent, rank)
    end
    ans << count
  end
  ans
end

data = STDIN.read.split.map(&:to_i)
exit if data.empty?

idx = 0
t = data[idx]
idx += 1
blocks = []
t.times do
  m = data[idx]
  n = data[idx + 1]
  k = data[idx + 2]
  idx += 3
  positions = []
  k.times do
    positions << [data[idx], data[idx + 1]]
    idx += 2
  end
  blocks << "[#{solve(m, n, positions).join(',')}]"
end

print blocks.join("\n\n")
