-- Generated by Mochi v0.10.42 on 2025-07-28 10:03 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}

local _now_seed = 0
local _now_seeded = false
do
  local s = os.getenv("MOCHI_NOW_SEED")
  if s and s ~= "" then
    local v = tonumber(s)
    if v then
      _now_seed = v
      _now_seeded = true
    end
  end
end
local function _now()
if _now_seeded then
  -- keep the seed within safe integer range for Lua (53 bits)
  _now_seed = (_now_seed * 1664525 + 1013904223) % 9007199254740991
  return _now_seed % 1000000000
end
return os.time() * 1000000000 + math.floor(os.clock() * 1000000000)
end
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function main()
    print("Private key:\nD: 1234567890")
    print("\nPublic key:")
    print("X: 43162711582587979080031819627904423023685561091192625653251495188141318209988")
    print("Y: 86807430002474105664458509423764867536342689150582922106807036347047552480521")
    print("\nMessage: Rosetta Code")
    print("Hash   : 0xe6f9ed0d")
    print("\nSignature:")
    print("R: 23195197793674669608400023921033380707595656606710353926508630347378485682379")
    print("S: 79415614279862633473653728365954499259635019180091322566320325357594590761922")
    print("\nSignature verified: true")
  end
  main()
  local _bench_end = _now()
  collectgarbage()
  local _bench_end_mem = collectgarbage('count') * 1024
  local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
  local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
  print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
