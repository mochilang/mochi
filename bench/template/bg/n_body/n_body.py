import json
import math
import time

DAYS_PER_YEAR = 365.24
SOLAR_MASS = 4 * math.pi * math.pi
DT = 0.01
N_BODIES = 5

steps = {{ .N }}

pos_x = [
    0.0,
    4.84143144246472090e+00,
    8.34336671824457987e+00,
    1.28943695621391310e+01,
    1.53796971148509165e+01,
]
pos_y = [
    0.0,
    -1.16032004402742839e+00,
    4.12479856412430479e+00,
    -1.51111514016986312e+01,
    -2.59193146099879641e+01,
]
pos_z = [
    0.0,
    -1.03622044471123109e-01,
    -4.03523417114321381e-01,
    -2.23307578892655734e-01,
    1.79258772950371181e-01,
]
vel_x = [
    0.0,
    1.66007664274403694e-03 * DAYS_PER_YEAR,
    -2.76742510726862411e-03 * DAYS_PER_YEAR,
    2.96460137564761618e-03 * DAYS_PER_YEAR,
    2.68067772490389322e-03 * DAYS_PER_YEAR,
]
vel_y = [
    0.0,
    7.69901118419740425e-03 * DAYS_PER_YEAR,
    4.99852801234917238e-03 * DAYS_PER_YEAR,
    2.37847173959480950e-03 * DAYS_PER_YEAR,
    1.62824170038242295e-03 * DAYS_PER_YEAR,
]
vel_z = [
    0.0,
    -6.90460016972063023e-05 * DAYS_PER_YEAR,
    2.30417297573763929e-05 * DAYS_PER_YEAR,
    -2.96589568540237556e-05 * DAYS_PER_YEAR,
    -9.51592254519715870e-05 * DAYS_PER_YEAR,
]
mass = [
    SOLAR_MASS,
    9.54791938424326609e-04 * SOLAR_MASS,
    2.85885980666130812e-04 * SOLAR_MASS,
    4.36624404335156298e-05 * SOLAR_MASS,
    5.15138902046611451e-05 * SOLAR_MASS,
]

px = 0.0
py = 0.0
pz = 0.0
for i in range(1, N_BODIES):
    px -= vel_x[i] * mass[i]
    py -= vel_y[i] * mass[i]
    pz -= vel_z[i] * mass[i]
vel_x[0] = px / SOLAR_MASS
vel_y[0] = py / SOLAR_MASS
vel_z[0] = pz / SOLAR_MASS

start = time.perf_counter()
for _ in range(steps):
    for i in range(N_BODIES):
        for j in range(i + 1, N_BODIES):
            dx = pos_x[i] - pos_x[j]
            dy = pos_y[i] - pos_y[j]
            dz = pos_z[i] - pos_z[j]
            d2 = dx * dx + dy * dy + dz * dz
            mag = DT / (d2 * math.sqrt(d2))
            mi_mag = mass[i] * mag
            mj_mag = mass[j] * mag
            vel_x[i] -= dx * mj_mag
            vel_y[i] -= dy * mj_mag
            vel_z[i] -= dz * mj_mag
            vel_x[j] += dx * mi_mag
            vel_y[j] += dy * mi_mag
            vel_z[j] += dz * mi_mag
    for i in range(N_BODIES):
        pos_x[i] += vel_x[i] * DT
        pos_y[i] += vel_y[i] * DT
        pos_z[i] += vel_z[i] * DT

energy = 0.0
for i in range(N_BODIES):
    kin = 0.5 * mass[i] * (vel_x[i] * vel_x[i] + vel_y[i] * vel_y[i] + vel_z[i] * vel_z[i])
    pot = 0.0
    for j in range(i + 1, N_BODIES):
        dx = pos_x[i] - pos_x[j]
        dy = pos_y[i] - pos_y[j]
        dz = pos_z[i] - pos_z[j]
        r = math.sqrt(dx * dx + dy * dy + dz * dz)
        pot += mass[i] * mass[j] / r
    energy += kin - pot

output = int(energy * 1e9)
duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": output,
}))
