-- Reference workload for crosslang comparison with the MEP-30 JIT.
-- Computes sum_{i=0..n-1} (i*2 + 3). Calibrates iters so total >= 1s,
-- reports ns/op. Works with both Lua 5.x and LuaJIT.

local function fillsum(n)
    local s = 0
    for i = 0, n - 1 do
        s = s + i * 2 + 3
    end
    return s
end

local function clock_ns()
    return os.clock() * 1e9
end

local function bench(n)
    local iters = 1
    while true do
        local t0 = clock_ns()
        for k = 1, iters do
            fillsum(n)
        end
        local dt = clock_ns() - t0
        if dt >= 1e9 then
            local ns = dt / iters
            io.write(string.format("fillsum_lua n=%d: %10.1f ns/op  (%d iters, %.2fs)\n",
                n, ns, iters, dt / 1e9))
            return
        end
        iters = iters * 2
    end
end

for _, n in ipairs({128, 1024, 10000}) do
    bench(n)
end
