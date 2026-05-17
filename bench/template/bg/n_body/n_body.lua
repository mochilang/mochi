-- MEP-39 n_body Lua peer. Canonical Benchmarks Game initial conditions
-- (Sun + Jupiter, Saturn, Uranus, Neptune), advance + posUpdate
-- iterated N times, final energy * 1e9 truncated toward zero.

local DAYS_PER_YEAR = 365.24
local SOLAR_MASS = 4.0 * math.pi * math.pi
local DT = 0.01
local N_BODIES = 5

local steps = {{ .N }}

local pos_x = {
  0.0,
  4.84143144246472090e+00,
  8.34336671824457987e+00,
  1.28943695621391310e+01,
  1.53796971148509165e+01,
}
local pos_y = {
  0.0,
  -1.16032004402742839e+00,
  4.12479856412430479e+00,
  -1.51111514016986312e+01,
  -2.59193146099879641e+01,
}
local pos_z = {
  0.0,
  -1.03622044471123109e-01,
  -4.03523417114321381e-01,
  -2.23307578892655734e-01,
  1.79258772950371181e-01,
}
local vel_x = {
  0.0,
  1.66007664274403694e-03 * DAYS_PER_YEAR,
  -2.76742510726862411e-03 * DAYS_PER_YEAR,
  2.96460137564761618e-03 * DAYS_PER_YEAR,
  2.68067772490389322e-03 * DAYS_PER_YEAR,
}
local vel_y = {
  0.0,
  7.69901118419740425e-03 * DAYS_PER_YEAR,
  4.99852801234917238e-03 * DAYS_PER_YEAR,
  2.37847173959480950e-03 * DAYS_PER_YEAR,
  1.62824170038242295e-03 * DAYS_PER_YEAR,
}
local vel_z = {
  0.0,
  -6.90460016972063023e-05 * DAYS_PER_YEAR,
  2.30417297573763929e-05 * DAYS_PER_YEAR,
  -2.96589568540237556e-05 * DAYS_PER_YEAR,
  -9.51592254519715870e-05 * DAYS_PER_YEAR,
}
local mass = {
  SOLAR_MASS,
  9.54791938424326609e-04 * SOLAR_MASS,
  2.85885980666130812e-04 * SOLAR_MASS,
  4.36624404335156298e-05 * SOLAR_MASS,
  5.15138902046611451e-05 * SOLAR_MASS,
}

local px, py, pz = 0.0, 0.0, 0.0
for i = 2, N_BODIES do
  px = px - vel_x[i] * mass[i]
  py = py - vel_y[i] * mass[i]
  pz = pz - vel_z[i] * mass[i]
end
vel_x[1] = px / SOLAR_MASS
vel_y[1] = py / SOLAR_MASS
vel_z[1] = pz / SOLAR_MASS

local function trunc(x)
  if x >= 0.0 then return math.floor(x) else return math.ceil(x) end
end

local start = os.clock()
for _ = 1, steps do
  for i = 1, N_BODIES do
    for j = i + 1, N_BODIES do
      local dx = pos_x[i] - pos_x[j]
      local dy = pos_y[i] - pos_y[j]
      local dz = pos_z[i] - pos_z[j]
      local d2 = dx * dx + dy * dy + dz * dz
      local mag = DT / (d2 * math.sqrt(d2))
      local mi_mag = mass[i] * mag
      local mj_mag = mass[j] * mag
      vel_x[i] = vel_x[i] - dx * mj_mag
      vel_y[i] = vel_y[i] - dy * mj_mag
      vel_z[i] = vel_z[i] - dz * mj_mag
      vel_x[j] = vel_x[j] + dx * mi_mag
      vel_y[j] = vel_y[j] + dy * mi_mag
      vel_z[j] = vel_z[j] + dz * mi_mag
    end
  end
  for i = 1, N_BODIES do
    pos_x[i] = pos_x[i] + vel_x[i] * DT
    pos_y[i] = pos_y[i] + vel_y[i] * DT
    pos_z[i] = pos_z[i] + vel_z[i] * DT
  end
end

local energy = 0.0
for i = 1, N_BODIES do
  local kin = 0.5 * mass[i] * (vel_x[i] * vel_x[i] + vel_y[i] * vel_y[i] + vel_z[i] * vel_z[i])
  local pot = 0.0
  for j = i + 1, N_BODIES do
    local dx = pos_x[i] - pos_x[j]
    local dy = pos_y[i] - pos_y[j]
    local dz = pos_z[i] - pos_z[j]
    local r = math.sqrt(dx * dx + dy * dy + dz * dz)
    pot = pot + mass[i] * mass[j] / r
  end
  energy = energy + kin - pot
end

local output = trunc(energy * 1e9)
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %d}\n', math.floor(duration_us), output))
