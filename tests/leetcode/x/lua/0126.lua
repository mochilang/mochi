local function ladders(beginWord, endWord, words)
  local wordSet = {}
  for _, w in ipairs(words) do wordSet[w] = true end
  if not wordSet[endWord] then return {} end
  local parents = {}
  local level = {[beginWord] = true}
  local visited = {[beginWord] = true}
  local found = false
  while next(level) and not found do
    local nextLevel = {}
    local cur = {}
    for w, _ in pairs(level) do table.insert(cur, w) end
    table.sort(cur)
    for _, word in ipairs(cur) do
      local chars = {word:byte(1, #word)}
      for i = 1, #chars do
        local orig = chars[i]
        for c = string.byte('a'), string.byte('z') do
          if c ~= orig then
            chars[i] = c
            local nw = string.char(table.unpack(chars))
            if wordSet[nw] and not visited[nw] then
              nextLevel[nw] = true
              if not parents[nw] then parents[nw] = {} end
              table.insert(parents[nw], word)
              if nw == endWord then found = true end
            end
          end
        end
        chars[i] = orig
      end
    end
    for w, _ in pairs(nextLevel) do visited[w] = true end
    level = nextLevel
  end
  if not found then return {} end
  local out, path = {}, {endWord}
  local function backtrack(word)
    if word == beginWord then
      local seq = {}
      for i = #path, 1, -1 do table.insert(seq, path[i]) end
      table.insert(out, seq)
      return
    end
    local plist = parents[word] or {}
    table.sort(plist)
    for _, p in ipairs(plist) do
      table.insert(path, p)
      backtrack(p)
      table.remove(path)
    end
  end
  backtrack(endWord)
  table.sort(out, function(a, b) return table.concat(a, '->') < table.concat(b, '->') end)
  return out
end
local function fmt(paths)
  local lines = {tostring(#paths)}
  for _, p in ipairs(paths) do table.insert(lines, table.concat(p, '->')) end
  return table.concat(lines, '\n')
end
local lines = {}
for line in io.lines() do table.insert(lines, line) end
if #lines == 0 then return end
local tc = tonumber(lines[1])
local idx, out = 2, {}
for _ = 1, tc do
  local beginWord = lines[idx]; idx = idx + 1
  local endWord = lines[idx]; idx = idx + 1
  local n = tonumber(lines[idx]); idx = idx + 1
  local words = {}
  for i = 1, n do words[i] = lines[idx]; idx = idx + 1 end
  table.insert(out, fmt(ladders(beginWord, endWord, words)))
end
io.write(table.concat(out, '\n\n'))
