package corpus

import (
	"math"

	"mochi/runtime/vm3"
)

// ExpectN_body is the Go oracle that mirrors the N_body kernel
// bit-for-bit: 5 bodies with simplified initial conditions
// (positions (i, 2i, 3i), velocities (i/10, i/5, 3i/10), masses
// i+1), `steps` advance+posUpdate iterations at dt=0.01, then
// system energy.
//
// The vm3 kernel and this function evaluate the floating-point
// ops in the same order, so the test asserts bit-equal results.
func ExpectN_body(steps int64) float64 {
	const N = 5
	const dt = 0.01
	var x, y, z, vx, vy, vz, m [N]float64
	for i := 0; i < N; i++ {
		fi := float64(i)
		x[i] = fi
		y[i] = fi * 2
		z[i] = fi * 3
		vx[i] = fi * 0.1
		vy[i] = fi * 0.2
		vz[i] = fi * 0.3
		m[i] = fi + 1
	}
	for s := int64(0); s < steps; s++ {
		for i := 0; i < N; i++ {
			for j := i + 1; j < N; j++ {
				dx := x[i] - x[j]
				dy := y[i] - y[j]
				dz := z[i] - z[j]
				d2 := dx*dx + dy*dy + dz*dz
				mag := dt / (d2 * math.Sqrt(d2))
				miMag := m[i] * mag
				mjMag := m[j] * mag
				vx[i] -= dx * mjMag
				vy[i] -= dy * mjMag
				vz[i] -= dz * mjMag
				vx[j] += dx * miMag
				vy[j] += dy * miMag
				vz[j] += dz * miMag
			}
		}
		for i := 0; i < N; i++ {
			x[i] += vx[i] * dt
			y[i] += vy[i] * dt
			z[i] += vz[i] * dt
		}
	}
	var e float64
	for i := 0; i < N; i++ {
		kin := 0.5 * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i])
		var pot float64
		for j := i + 1; j < N; j++ {
			dx := x[i] - x[j]
			dy := y[i] - y[j]
			dz := z[i] - z[j]
			r := math.Sqrt(dx*dx + dy*dy + dz*dz)
			pot += m[i] * m[j] / r
		}
		e += kin - pot
	}
	return e
}

// N_body: BG `n_body` simulator port from compiler2/corpus.BuildNBodyKernel.
// Uses the same simplified test initial conditions (positions
// (i, 2i, 3i), velocities (i/10, i/5, 3i/10), masses i+1) so the Go
// oracle in compiler3/corpus.ExpectN_body stays compact and the
// bench is a fair vm3-vs-Go comparison. Build(n) returns a one-fn
// program that runs n advance+posUpdate steps then computes total
// system energy and returns it as f64.
//
// Phase 6.3.4.j.5.c migrates the seven body arrays from generic
// Cell-backed lists to typed vm3.F64Array values. OpNewF64Array
// pre-allocates a fixed 5-slot array per body component, so the
// push_loop that seeded zeros in the list version is dropped (the
// arena hands back length-N storage already cleared). OpF64ArrayGetF64
// and OpF64ArraySetF64 replace the OpListGetF64/SetF64 pairs.
// On ARM64 the JIT lowers all three ops inline through the f64arrs
// slab pinned in x19 (Phase 6.3.4.j.5.b); on AMD64 the kernel runs
// through the interpreter until j.5.d adds AMD64 lowering.
//
// Register banks (NumRegs: 7 / 8 / 7):
//
//	I64: 0 steps_in, 1 s, 2 i, 3 j, 4 p, 5 bi, 6 bj
//	     (reg 6 only carries bj in the energy phase; no other
//	     overlap since push_loop is gone)
//	F64: 0..7 working file (reused across phases)
//	Cell: 0 pos_x, 1 pos_y, 2 pos_z, 3 vel_x, 4 vel_y, 5 vel_z, 6 mass
//
// Consts (f64): [0]=0.0 [1]=1.0 [2]=2.0 [3]=3.0 [4]=0.1 [5]=0.2 [6]=0.3
// [7]=0.01 (dt) [8]=0.5.
//
// PC map (154 ops; see the bytecode literal below):
//
//	0..6     NewF64Array(5) for the 7 typed arrays
//	7..30    init_loop: x[i]=fi; y=2fi; z=3fi; vx=0.1fi; vy=0.2fi; vz=0.3fi; m=fi+1
//	31..108  step_loop: advance (adv_i x adv_j) + posUpdate
//	109..152 energy loop (kin per body + pairwise pot via sqrt)
//	153      ReturnF64
//
// Branch targets (uint16(C)):
//
//	init_loop  pc=8   -> init_done  pc=31
//	step_loop  pc=32  -> step_done  pc=109
//	adv_i_loop pc=34  -> adv_i_done pc=87
//	adv_j_loop pc=36  -> adv_j_done pc=85
//	pos_loop   pc=88  -> pos_done   pc=107
//	energy_loop pc=111 -> energy_done pc=153
//	pot_loop   pc=126 -> pot_done    pc=149
var N_body = &Program{
	Name: "n_body",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "n_body",
			NumRegsI64:  7,
			NumRegsF64:  8,
			NumRegsCell: 7,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankF64,
			Consts: []vm3.Cell{
				vm3.CFloat(0.0),  // [0]
				vm3.CFloat(1.0),  // [1]
				vm3.CFloat(2.0),  // [2]
				vm3.CFloat(3.0),  // [3]
				vm3.CFloat(0.1),  // [4]
				vm3.CFloat(0.2),  // [5]
				vm3.CFloat(0.3),  // [6]
				vm3.CFloat(0.01), // [7] dt
				vm3.CFloat(0.5),  // [8]
			},
			Code: []vm3.Op{
				// --- setup: 7 NewF64Array(5) (pc 0..6)
				vm3.MakeOp(vm3.OpNewF64Array, 0, 0, 5),
				vm3.MakeOp(vm3.OpNewF64Array, 1, 0, 5),
				vm3.MakeOp(vm3.OpNewF64Array, 2, 0, 5),
				vm3.MakeOp(vm3.OpNewF64Array, 3, 0, 5),
				vm3.MakeOp(vm3.OpNewF64Array, 4, 0, 5),
				vm3.MakeOp(vm3.OpNewF64Array, 5, 0, 5),
				vm3.MakeOp(vm3.OpNewF64Array, 6, 0, 5),
				// init_loop pc=7
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0), // pc=7: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 2, 5, 31), // pc=8: if i>=5 -> init_done
				vm3.MakeOp(vm3.OpI64ToF64, 0, 2, 0),     // pc=9: f0 = fi
				// pos_x[i] = fi
				vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 2), // pc=10
				// pos_y[i] = fi * 2
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 2),      // pc=11: f1 = 2.0
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),         // pc=12: f1 = fi * 2
				vm3.MakeOp(vm3.OpF64ArraySetF64, 1, 1, 2), // pc=13
				// pos_z[i] = fi * 3
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 3),      // pc=14: f1 = 3.0
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),         // pc=15: f1 = fi * 3
				vm3.MakeOp(vm3.OpF64ArraySetF64, 2, 1, 2), // pc=16
				// vel_x[i] = fi * 0.1
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 4),      // pc=17
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),         // pc=18
				vm3.MakeOp(vm3.OpF64ArraySetF64, 3, 1, 2), // pc=19
				// vel_y[i] = fi * 0.2
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 5),      // pc=20
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),         // pc=21
				vm3.MakeOp(vm3.OpF64ArraySetF64, 4, 1, 2), // pc=22
				// vel_z[i] = fi * 0.3
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 6),      // pc=23
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),         // pc=24
				vm3.MakeOp(vm3.OpF64ArraySetF64, 5, 1, 2), // pc=25
				// mass[i] = fi + 1
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 1),      // pc=26: f1 = 1.0
				vm3.MakeOp(vm3.OpAddF64, 1, 0, 1),         // pc=27: f1 = fi + 1
				vm3.MakeOp(vm3.OpF64ArraySetF64, 6, 1, 2), // pc=28
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),        // pc=29: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 8),           // pc=30: -> init_loop
				// init_done pc=31
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0), // pc=31: s = 0
				// step_loop pc=32
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 109), // pc=32: if s >= steps_in -> step_done
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),    // pc=33: i = 0
				// adv_i_loop pc=34
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 2, 5, 87), // pc=34: if i>=5 -> adv_i_done
				vm3.MakeOp(vm3.OpAddI64K, 3, 2, 1),      // pc=35: j = i + 1
				// adv_j_loop pc=36
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 3, 5, 85), // pc=36: if j>=5 -> adv_j_done
				// dx = pos_x[i] - pos_x[j]  (regs: f0=dx)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 0, 0, 2), // pc=37
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 0, 3), // pc=38
				vm3.MakeOp(vm3.OpSubF64, 0, 0, 1),         // pc=39
				// dy (f1)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 1, 2), // pc=40
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 1, 3), // pc=41: scratch f2
				vm3.MakeOp(vm3.OpSubF64, 1, 1, 2),         // pc=42: f1 = dy
				// dz (f2)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 2, 2), // pc=43
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 3, 2, 3), // pc=44: scratch f3
				vm3.MakeOp(vm3.OpSubF64, 2, 2, 3),         // pc=45: f2 = dz
				// d2 = dx*dx + dy*dy + dz*dz  (reuse f3)
				vm3.MakeOp(vm3.OpMulF64, 3, 0, 0), // pc=46: dx^2
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 1), // pc=47: dy^2
				vm3.MakeOp(vm3.OpMulF64, 5, 2, 2), // pc=48: dz^2
				vm3.MakeOp(vm3.OpAddF64, 3, 3, 4), // pc=49
				vm3.MakeOp(vm3.OpAddF64, 3, 3, 5), // pc=50: f3 = d2
				// mag = dt / (d2 * sqrt(d2))  (f4=mag)
				vm3.MakeOp(vm3.OpSqrtF64, 4, 3, 0),        // pc=51: f4 = sqrt(d2)
				vm3.MakeOp(vm3.OpMulF64, 4, 3, 4),         // pc=52: f4 = d2 * sqrt(d2)
				vm3.MakeOp(vm3.OpConstF64K, 5, 0, 7),      // pc=53: f5 = dt
				vm3.MakeOp(vm3.OpDivF64, 4, 5, 4),         // pc=54: f4 = mag
				// mi_mag = m[i] * mag  (f5);  mj_mag = m[j] * mag (f6)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 5, 6, 2), // pc=55: m[i]
				vm3.MakeOp(vm3.OpMulF64, 5, 5, 4),         // pc=56: mi_mag
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 6, 6, 3), // pc=57: m[j]
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 4),         // pc=58: mj_mag
				// vx[i] -= dx * mj_mag  (use f7 scratch, reuse f4)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 3, 2), // pc=59
				vm3.MakeOp(vm3.OpMulF64, 4, 0, 6),         // pc=60: dx*mj_mag
				vm3.MakeOp(vm3.OpSubF64, 7, 7, 4),         // pc=61
				vm3.MakeOp(vm3.OpF64ArraySetF64, 3, 7, 2), // pc=62
				// vy[i] -= dy * mj_mag
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 4, 2), // pc=63
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 6),         // pc=64
				vm3.MakeOp(vm3.OpSubF64, 7, 7, 4),         // pc=65
				vm3.MakeOp(vm3.OpF64ArraySetF64, 4, 7, 2), // pc=66
				// vz[i] -= dz * mj_mag
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 5, 2), // pc=67
				vm3.MakeOp(vm3.OpMulF64, 4, 2, 6),         // pc=68
				vm3.MakeOp(vm3.OpSubF64, 7, 7, 4),         // pc=69
				vm3.MakeOp(vm3.OpF64ArraySetF64, 5, 7, 2), // pc=70
				// vx[j] += dx * mi_mag
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 3, 3), // pc=71
				vm3.MakeOp(vm3.OpMulF64, 4, 0, 5),         // pc=72
				vm3.MakeOp(vm3.OpAddF64, 7, 7, 4),         // pc=73
				vm3.MakeOp(vm3.OpF64ArraySetF64, 3, 7, 3), // pc=74
				// vy[j] += dy * mi_mag
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 4, 3), // pc=75
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 5),         // pc=76
				vm3.MakeOp(vm3.OpAddF64, 7, 7, 4),         // pc=77
				vm3.MakeOp(vm3.OpF64ArraySetF64, 4, 7, 3), // pc=78
				// vz[j] += dz * mi_mag
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 5, 3), // pc=79
				vm3.MakeOp(vm3.OpMulF64, 4, 2, 5),         // pc=80
				vm3.MakeOp(vm3.OpAddF64, 7, 7, 4),         // pc=81
				vm3.MakeOp(vm3.OpF64ArraySetF64, 5, 7, 3), // pc=82
				// j++ ; -> adv_j_loop
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1), // pc=83
				vm3.MakeOp(vm3.OpJump, 0, 0, 36),   // pc=84
				// adv_j_done pc=85
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1), // pc=85
				vm3.MakeOp(vm3.OpJump, 0, 0, 34),   // pc=86 -> adv_i_loop
				// adv_i_done pc=87
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0), // pc=87: p = 0
				// pos_loop pc=88
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 4, 5, 107), // pc=88: if p>=5 -> pos_done
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 7),     // pc=89: f0 = dt
				// pos_x[p] += vel_x[p] * dt
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 3, 4), // pc=90: f1 = vx
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 0),         // pc=91: f1 = vx*dt
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 0, 4), // pc=92: f2 = px
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 1),         // pc=93
				vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 2, 4), // pc=94
				// pos_y[p] += vel_y[p] * dt
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 4, 4), // pc=95
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 0),         // pc=96
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 1, 4), // pc=97
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 1),         // pc=98
				vm3.MakeOp(vm3.OpF64ArraySetF64, 1, 2, 4), // pc=99
				// pos_z[p] += vel_z[p] * dt
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 5, 4), // pc=100
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 0),         // pc=101
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 2, 4), // pc=102
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 1),         // pc=103
				vm3.MakeOp(vm3.OpF64ArraySetF64, 2, 2, 4), // pc=104
				vm3.MakeOp(vm3.OpAddI64K, 4, 4, 1),        // pc=105: p++
				vm3.MakeOp(vm3.OpJump, 0, 0, 88),          // pc=106 -> pos_loop
				// pos_done pc=107
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1), // pc=107: s++
				vm3.MakeOp(vm3.OpJump, 0, 0, 32),   // pc=108 -> step_loop
				// step_done pc=109: energy accumulator f0
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0), // pc=109: e = 0.0
				vm3.MakeOp(vm3.OpConstI64K, 5, 0, 0), // pc=110: bi = 0
				// energy_loop pc=111
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 5, 5, 153), // pc=111: if bi>=5 -> end
				// vsq = vx[bi]^2 + vy[bi]^2 + vz[bi]^2 (f2 accumulator)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 3, 5), // pc=112: vx
				vm3.MakeOp(vm3.OpMulF64, 2, 1, 1),         // pc=113: vx^2
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 4, 5), // pc=114: vy
				vm3.MakeOp(vm3.OpMulF64, 3, 1, 1),         // pc=115: vy^2
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 3),         // pc=116
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 5, 5), // pc=117: vz
				vm3.MakeOp(vm3.OpMulF64, 3, 1, 1),         // pc=118: vz^2
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 3),         // pc=119: f2 = vsq
				// kin = 0.5 * m[bi] * vsq  (f3)
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 3, 6, 5), // pc=120: m[bi]
				vm3.MakeOp(vm3.OpConstF64K, 4, 0, 8),      // pc=121: f4 = 0.5
				vm3.MakeOp(vm3.OpMulF64, 3, 3, 4),         // pc=122: 0.5*m
				vm3.MakeOp(vm3.OpMulF64, 3, 3, 2),         // pc=123: kin
				// pot = 0 (f4); bj = bi + 1
				vm3.MakeOp(vm3.OpConstF64K, 4, 0, 0), // pc=124: pot = 0
				vm3.MakeOp(vm3.OpAddI64K, 6, 5, 1),   // pc=125: bj = bi + 1
				// pot_loop pc=126
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 6, 5, 149), // pc=126: if bj>=5 -> pot_done
				// dx (f5 scratch), accumulate squared distance in f5
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 5, 0, 5), // pc=127: xi
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 6, 0, 6), // pc=128: xj
				vm3.MakeOp(vm3.OpSubF64, 5, 5, 6),         // pc=129: f5 = dx
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 6, 1, 5), // pc=130: yi
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 1, 6), // pc=131: yj
				vm3.MakeOp(vm3.OpSubF64, 6, 6, 7),         // pc=132: f6 = dy
				vm3.MakeOp(vm3.OpMulF64, 5, 5, 5),         // pc=133: dx^2
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 6),         // pc=134: dy^2
				vm3.MakeOp(vm3.OpAddF64, 5, 5, 6),         // pc=135: dx^2+dy^2
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 6, 2, 5), // pc=136: zi
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 2, 6), // pc=137: zj
				vm3.MakeOp(vm3.OpSubF64, 6, 6, 7),         // pc=138: f6 = dz
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 6),         // pc=139: dz^2
				vm3.MakeOp(vm3.OpAddF64, 5, 5, 6),         // pc=140: f5 = d2
				vm3.MakeOp(vm3.OpSqrtF64, 5, 5, 0),        // pc=141: f5 = r
				// m[bi] * m[bj] / r ; pot += ...
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 6, 6, 5), // pc=142: m[bi]
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 7, 6, 6), // pc=143: m[bj]
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 7),         // pc=144: m[bi]*m[bj]
				vm3.MakeOp(vm3.OpDivF64, 6, 6, 5),         // pc=145: /r
				vm3.MakeOp(vm3.OpAddF64, 4, 4, 6),         // pc=146: pot += ...
				vm3.MakeOp(vm3.OpAddI64K, 6, 6, 1),        // pc=147: bj++
				vm3.MakeOp(vm3.OpJump, 0, 0, 126),         // pc=148 -> pot_loop
				// pot_done pc=149
				vm3.MakeOp(vm3.OpSubF64, 3, 3, 4),  // pc=149: kin - pot
				vm3.MakeOp(vm3.OpAddF64, 0, 0, 3),  // pc=150: e += kin - pot
				vm3.MakeOp(vm3.OpAddI64K, 5, 5, 1), // pc=151: bi++
				vm3.MakeOp(vm3.OpJump, 0, 0, 111),  // pc=152 -> energy_loop
				// end pc=153
				vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0), // pc=153
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
