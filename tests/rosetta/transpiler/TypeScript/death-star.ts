// Generated by Mochi v0.10.42 on 2025-07-27 21:21:00 GMT+7

export interface V3 { x: number; y: number; z: number }
export interface Sphere { cx: number; cy: number; cz: number; r: number }
function sqrtApprox(x: number): number {
  if ((x <= 0.0)) {
    return 0.0;
  }
  let guess: number = x;
  let i: number = 0;
  while ((i < 20)) {
    guess = ((guess + (x / guess)) / 2.0);
    i = (i + 1);
  }
  return guess;
}
function powf(base: number, exp: number): number {
  let result: number = 1.0;
  let i: number = 0;
  while ((i < exp)) {
    result = (result * base);
    i = (i + 1);
  }
  return result;
}
function normalize(v: V3): V3 {
  let len = sqrtApprox((((v.x * v.x) + (v.y * v.y)) + (v.z * v.z)));
  return {"x": (v.x / len), "y": (v.y / len), "z": (v.z / len)};
}
function dot(a: V3, b: V3): number {
  let d: number = (((a.x * b.x) + (a.y * b.y)) + (a.z * b.z));
  if ((d < 0.0)) {
    return -d;
  }
  return 0.0;
}
function hitSphere(s: Sphere, x: number, y: number): Record<string, any> {
  let dx: number = (x - s.cx);
  let dy: number = (y - s.cy);
  let zsq: number = ((s.r * s.r) - ((dx * dx) + (dy * dy)));
  if ((zsq < 0.0)) {
    return {"hit": false};
  }
  let z: number = sqrtApprox(zsq);
  return {"hit": true, "z1": (s.cz - z), "z2": (s.cz + z)};
}
function main() {
  let shades: string = ".:!*oe&#%@";
  let light: V3 = normalize({"x": -50.0, "y": 30.0, "z": 50.0});
  let pos: Sphere = {"cx": 20.0, "cy": 20.0, "cz": 0.0, "r": 20.0};
  let neg: Sphere = {"cx": 1.0, "cy": 1.0, "cz": -6.0, "r": 20.0};
  let yi: number = 0;
  while ((yi <= 40)) {
    let y: number = (yi + 0.5);
    let line: string = "";
    let xi: number = -20;
    while ((xi <= 60)) {
      let x: number = ((((xi - pos.cx) / 2.0) + 0.5) + pos.cx);
      let hb: Record<string, any> = hitSphere(pos, x, y);
      if (!hb.hit) {
        line = (line + " ");
        xi = (xi + 1);
        continue
      }
      let zb1 = hb.z1;
      let zb2 = hb.z2;
      let hs: Record<string, any> = hitSphere(neg, x, y);
      let hitRes: number = 1;
      if (!hs.hit) {
        hitRes = 1;
      } else {
        if ((hs.z1 > zb1)) {
          hitRes = 1;
        } else {
          if ((hs.z2 > zb2)) {
            hitRes = 0;
          } else {
            if ((hs.z2 > zb1)) {
              hitRes = 2;
            } else {
              hitRes = 1;
            }
          }
        }
      }
      if ((hitRes == 0)) {
        line = (line + " ");
        xi = (xi + 1);
        continue
      }
      let vec: V3 = {};
      if ((hitRes == 1)) {
        vec = {"x": (x - pos.cx), "y": (y - pos.cy), "z": (zb1 - pos.cz)};
      } else {
        vec = {"x": (neg.cx - x), "y": (neg.cy - y), "z": (neg.cz - hs.z2)};
      }
      vec = normalize(vec);
      let b: number = (powf(dot(light, vec), 2) + 0.5);
      let intensity: number = Math.trunc(((1.0 - b) * (Array.isArray(shades) || typeof shades === 'string' ? shades.length : Object.keys(shades ?? {}).length)));
      if ((intensity < 0)) {
        intensity = 0;
      }
      if ((intensity >= (Array.isArray(shades) || typeof shades === 'string' ? shades.length : Object.keys(shades ?? {}).length))) {
        intensity = ((Array.isArray(shades) || typeof shades === 'string' ? shades.length : Object.keys(shades ?? {}).length) - 1);
      }
      line = (line + (shades).substring(intensity, (intensity + 1)));
      xi = (xi + 1);
    }
    console.log(line);
    yi = (yi + 1);
  }
}
var _nowSeed = 0;
var _nowSeeded = false;
{
  let s = "";
  if (typeof Deno !== "undefined") {
    try {
      s = Deno.env.get("MOCHI_NOW_SEED") ?? "";
    } catch (_e) {
      s = "";
    }
  } else if (typeof process !== "undefined") {
    s = process.env.MOCHI_NOW_SEED || "";
  }
  if (s) {
    const v = parseInt(s, 10);
    if (!isNaN(v)) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  }
}
function _now(): number {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  if (typeof Deno !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  if (typeof performance !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  return Date.now() * 1000;
}
function _mem(): number {
  if (typeof Deno !== 'undefined') {
    return (Deno.memoryUsage?.().rss ?? 0);
  }
  if (typeof process !== 'undefined') {
    return process.memoryUsage().rss;
  }
  return 0;
}
(() => {
  const _startMem = _mem()
  const _start = _now()
  main();
  const _end = _now()
  const _duration = _end - _start
  const _duration_us = Math.trunc(_duration / 1000)
  const _endMem = _mem()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
})();

