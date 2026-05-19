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
// The kernel uses Cell-backed f64 lists (7 of them: pos_x, pos_y,
// pos_z, vel_x, vel_y, vel_z, mass) routed through OpListGetF64 /
// OpListSetF64 (landed in Phase 6.3.4.j.1). vm2 used a typed
// TF64Array primitive; vm3 has only generic Cell lists, and that's
// the surface this port targets so the JIT lowering in Phase
// 6.3.4.j.3 can stay inside the existing cells-slab hoist.
//
// Register banks (NumRegs: 9 / 8 / 7):
//
//	I64: 0 steps_in, 1 s, 2 i, 3 j, 4 p, 5 bi, 6 bj, 7 push_zero, 8 unused
//	F64: 0..7 working file (reused across phases)
//	Cell: 0 pos_x, 1 pos_y, 2 pos_z, 3 vel_x, 4 vel_y, 5 vel_z, 6 mass
//
// Consts (f64): [0]=0.0 [1]=1.0 [2]=2.0 [3]=3.0 [4]=0.1 [5]=0.2 [6]=0.3
// [7]=0.01 (dt) [8]=0.5.
//
// PC map (165 ops; see the bytecode literal below):
//
//	0..6   NewList for the 7 arrays
//	7..18  push 5 zeros into each list via push_loop
//	19..42 init_loop: x[i]=fi; y=2fi; z=3fi; vx=0.1fi; vy=0.2fi; vz=0.3fi; m=fi+1
//	43..120 step_loop: advance (adv_i x adv_j) + posUpdate
//	121..164 energy loop (kin per body + pairwise pot via sqrt)
//	165    ReturnF64
//
// Branch targets (uint16(C)):
//
//	push_loop pc=9   -> push_done pc=19
//	init_loop pc=20  -> init_done pc=43
//	step_loop pc=44  -> step_done pc=121
//	adv_i_loop pc=46 -> adv_i_done pc=99
//	adv_j_loop pc=48 -> adv_j_done pc=97
//	pos_loop  pc=100 -> pos_done  pc=119
//	energy_loop pc=123 -> energy_done pc=165
//	pot_loop  pc=138 -> pot_done  pc=161
//
// FmaF64 fusion is left to a follow-up; the spec §6.3.4.h FMA op needs
// the multiplicand and addend in distinct f64 regs, and the inner loop
// already operates near the 8-reg cap.
var N_body = &Program{
	Name: "n_body",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:        "n_body",
			NumRegsI64:  9,
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
				// --- setup: 7 NewList (pc 0..6)
				vm3.MakeOp(vm3.OpNewList, 0, 0, 0),
				vm3.MakeOp(vm3.OpNewList, 1, 0, 0),
				vm3.MakeOp(vm3.OpNewList, 2, 0, 0),
				vm3.MakeOp(vm3.OpNewList, 3, 0, 0),
				vm3.MakeOp(vm3.OpNewList, 4, 0, 0),
				vm3.MakeOp(vm3.OpNewList, 5, 0, 0),
				vm3.MakeOp(vm3.OpNewList, 6, 0, 0),
				// --- push 5 zeros into each list (pc 7..18). i64 reg 7 holds 0; reg 2 is loop i.
				vm3.MakeOp(vm3.OpConstI64K, 7, 0, 0), // pc=7: tmp_zero = 0
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0), // pc=8: i = 0
				// push_loop pc=9
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 2, 5, 19), // pc=9: if i>=5 -> push_done
				vm3.MakeOp(vm3.OpListPushI64, 0, 7, 0),  // pc=10
				vm3.MakeOp(vm3.OpListPushI64, 1, 7, 0),  // pc=11
				vm3.MakeOp(vm3.OpListPushI64, 2, 7, 0),  // pc=12
				vm3.MakeOp(vm3.OpListPushI64, 3, 7, 0),  // pc=13
				vm3.MakeOp(vm3.OpListPushI64, 4, 7, 0),  // pc=14
				vm3.MakeOp(vm3.OpListPushI64, 5, 7, 0),  // pc=15
				vm3.MakeOp(vm3.OpListPushI64, 6, 7, 0),  // pc=16
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),      // pc=17: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 9),         // pc=18: -> push_loop
				// push_done pc=19
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0), // pc=19: i = 0
				// init_loop pc=20
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 2, 5, 43), // pc=20: if i>=5 -> init_done
				vm3.MakeOp(vm3.OpI64ToF64, 0, 2, 0),     // pc=21: f0 = fi
				// pos_x[i] = fi
				vm3.MakeOp(vm3.OpListSetF64, 0, 0, 2), // pc=22
				// pos_y[i] = fi * 2
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 2),  // pc=23: f1 = 2.0
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),     // pc=24: f1 = fi * 2
				vm3.MakeOp(vm3.OpListSetF64, 1, 1, 2), // pc=25
				// pos_z[i] = fi * 3
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 3),  // pc=26: f1 = 3.0
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),     // pc=27: f1 = fi * 3
				vm3.MakeOp(vm3.OpListSetF64, 2, 1, 2), // pc=28
				// vel_x[i] = fi * 0.1
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 4),  // pc=29
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),     // pc=30
				vm3.MakeOp(vm3.OpListSetF64, 3, 1, 2), // pc=31
				// vel_y[i] = fi * 0.2
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 5),  // pc=32
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),     // pc=33
				vm3.MakeOp(vm3.OpListSetF64, 4, 1, 2), // pc=34
				// vel_z[i] = fi * 0.3
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 6),  // pc=35
				vm3.MakeOp(vm3.OpMulF64, 1, 0, 1),     // pc=36
				vm3.MakeOp(vm3.OpListSetF64, 5, 1, 2), // pc=37
				// mass[i] = fi + 1
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 1),  // pc=38: f1 = 1.0
				vm3.MakeOp(vm3.OpAddF64, 1, 0, 1),     // pc=39: f1 = fi + 1
				vm3.MakeOp(vm3.OpListSetF64, 6, 1, 2), // pc=40
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),    // pc=41: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 20),      // pc=42: -> init_loop
				// init_done pc=43
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0), // pc=43: s = 0
				// step_loop pc=44
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 121), // pc=44: if s >= steps_in -> step_done
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),    // pc=45: i = 0
				// adv_i_loop pc=46
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 2, 5, 99), // pc=46: if i>=5 -> adv_i_done
				vm3.MakeOp(vm3.OpAddI64K, 3, 2, 1),      // pc=47: j = i + 1
				// adv_j_loop pc=48
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 3, 5, 97), // pc=48: if j>=5 -> adv_j_done
				// dx = pos_x[i] - pos_x[j]  (regs: f0=dx)
				vm3.MakeOp(vm3.OpListGetF64, 0, 0, 2), // pc=49
				vm3.MakeOp(vm3.OpListGetF64, 1, 0, 3), // pc=50
				vm3.MakeOp(vm3.OpSubF64, 0, 0, 1),     // pc=51
				// dy (f1)
				vm3.MakeOp(vm3.OpListGetF64, 1, 1, 2), // pc=52
				vm3.MakeOp(vm3.OpListGetF64, 2, 1, 3), // pc=53: scratch f2
				vm3.MakeOp(vm3.OpSubF64, 1, 1, 2),     // pc=54: f1 = dy
				// dz (f2)
				vm3.MakeOp(vm3.OpListGetF64, 2, 2, 2), // pc=55
				vm3.MakeOp(vm3.OpListGetF64, 3, 2, 3), // pc=56: scratch f3
				vm3.MakeOp(vm3.OpSubF64, 2, 2, 3),     // pc=57: f2 = dz
				// d2 = dx*dx + dy*dy + dz*dz  (reuse f3)
				vm3.MakeOp(vm3.OpMulF64, 3, 0, 0), // pc=58: dx^2
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 1), // pc=59: dy^2
				vm3.MakeOp(vm3.OpMulF64, 5, 2, 2), // pc=60: dz^2
				vm3.MakeOp(vm3.OpAddF64, 3, 3, 4), // pc=61
				vm3.MakeOp(vm3.OpAddF64, 3, 3, 5), // pc=62: f3 = d2
				// mag = dt / (d2 * sqrt(d2))  (f4=mag)
				vm3.MakeOp(vm3.OpSqrtF64, 4, 3, 0),    // pc=63: f4 = sqrt(d2)
				vm3.MakeOp(vm3.OpMulF64, 4, 3, 4),     // pc=64: f4 = d2 * sqrt(d2)
				vm3.MakeOp(vm3.OpConstF64K, 5, 0, 7),  // pc=65: f5 = dt
				vm3.MakeOp(vm3.OpDivF64, 4, 5, 4),     // pc=66: f4 = mag
				// mi_mag = m[i] * mag  (f5);  mj_mag = m[j] * mag (f6)
				vm3.MakeOp(vm3.OpListGetF64, 5, 6, 2), // pc=67: m[i]
				vm3.MakeOp(vm3.OpMulF64, 5, 5, 4),     // pc=68: mi_mag
				vm3.MakeOp(vm3.OpListGetF64, 6, 6, 3), // pc=69: m[j]
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 4),     // pc=70: mj_mag
				// vx[i] -= dx * mj_mag  (use f7 scratch, reuse f4)
				vm3.MakeOp(vm3.OpListGetF64, 7, 3, 2), // pc=71
				vm3.MakeOp(vm3.OpMulF64, 4, 0, 6),     // pc=72: dx*mj_mag (f4 free since mag done)
				vm3.MakeOp(vm3.OpSubF64, 7, 7, 4),     // pc=73
				vm3.MakeOp(vm3.OpListSetF64, 3, 7, 2), // pc=74
				// vy[i] -= dy * mj_mag
				vm3.MakeOp(vm3.OpListGetF64, 7, 4, 2), // pc=75
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 6),     // pc=76
				vm3.MakeOp(vm3.OpSubF64, 7, 7, 4),     // pc=77
				vm3.MakeOp(vm3.OpListSetF64, 4, 7, 2), // pc=78
				// vz[i] -= dz * mj_mag
				vm3.MakeOp(vm3.OpListGetF64, 7, 5, 2), // pc=79
				vm3.MakeOp(vm3.OpMulF64, 4, 2, 6),     // pc=80
				vm3.MakeOp(vm3.OpSubF64, 7, 7, 4),     // pc=81
				vm3.MakeOp(vm3.OpListSetF64, 5, 7, 2), // pc=82
				// vx[j] += dx * mi_mag
				vm3.MakeOp(vm3.OpListGetF64, 7, 3, 3), // pc=83
				vm3.MakeOp(vm3.OpMulF64, 4, 0, 5),     // pc=84
				vm3.MakeOp(vm3.OpAddF64, 7, 7, 4),     // pc=85
				vm3.MakeOp(vm3.OpListSetF64, 3, 7, 3), // pc=86
				// vy[j] += dy * mi_mag
				vm3.MakeOp(vm3.OpListGetF64, 7, 4, 3), // pc=87
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 5),     // pc=88
				vm3.MakeOp(vm3.OpAddF64, 7, 7, 4),     // pc=89
				vm3.MakeOp(vm3.OpListSetF64, 4, 7, 3), // pc=90
				// vz[j] += dz * mi_mag
				vm3.MakeOp(vm3.OpListGetF64, 7, 5, 3), // pc=91
				vm3.MakeOp(vm3.OpMulF64, 4, 2, 5),     // pc=92
				vm3.MakeOp(vm3.OpAddF64, 7, 7, 4),     // pc=93
				vm3.MakeOp(vm3.OpListSetF64, 5, 7, 3), // pc=94
				// j++ ; -> adv_j_loop
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1), // pc=95
				vm3.MakeOp(vm3.OpJump, 0, 0, 48),   // pc=96
				// adv_j_done pc=97
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1), // pc=97
				vm3.MakeOp(vm3.OpJump, 0, 0, 46),   // pc=98 -> adv_i_loop
				// adv_i_done pc=99
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0), // pc=99: p = 0
				// pos_loop pc=100
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 4, 5, 119), // pc=100: if p>=5 -> pos_done
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 7),     // pc=101: f0 = dt
				// pos_x[p] += vel_x[p] * dt
				vm3.MakeOp(vm3.OpListGetF64, 1, 3, 4), // pc=102: f1 = vx
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 0),     // pc=103: f1 = vx*dt
				vm3.MakeOp(vm3.OpListGetF64, 2, 0, 4), // pc=104: f2 = px
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 1),     // pc=105
				vm3.MakeOp(vm3.OpListSetF64, 0, 2, 4), // pc=106
				// pos_y[p] += vel_y[p] * dt
				vm3.MakeOp(vm3.OpListGetF64, 1, 4, 4), // pc=107
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 0),     // pc=108
				vm3.MakeOp(vm3.OpListGetF64, 2, 1, 4), // pc=109
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 1),     // pc=110
				vm3.MakeOp(vm3.OpListSetF64, 1, 2, 4), // pc=111
				// pos_z[p] += vel_z[p] * dt
				vm3.MakeOp(vm3.OpListGetF64, 1, 5, 4), // pc=112
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 0),     // pc=113
				vm3.MakeOp(vm3.OpListGetF64, 2, 2, 4), // pc=114
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 1),     // pc=115
				vm3.MakeOp(vm3.OpListSetF64, 2, 2, 4), // pc=116
				vm3.MakeOp(vm3.OpAddI64K, 4, 4, 1),    // pc=117: p++
				vm3.MakeOp(vm3.OpJump, 0, 0, 100),     // pc=118 -> pos_loop
				// pos_done pc=119
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1), // pc=119: s++
				vm3.MakeOp(vm3.OpJump, 0, 0, 44),   // pc=120 -> step_loop
				// step_done pc=121: energy accumulator f0
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0), // pc=121: e = 0.0
				vm3.MakeOp(vm3.OpConstI64K, 5, 0, 0), // pc=122: bi = 0
				// energy_loop pc=123
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 5, 5, 165), // pc=123: if bi>=5 -> end
				// vsq = vx[bi]^2 + vy[bi]^2 + vz[bi]^2 (f2 accumulator)
				vm3.MakeOp(vm3.OpListGetF64, 1, 3, 5), // pc=124: vx
				vm3.MakeOp(vm3.OpMulF64, 2, 1, 1),     // pc=125: vx^2
				vm3.MakeOp(vm3.OpListGetF64, 1, 4, 5), // pc=126: vy
				vm3.MakeOp(vm3.OpMulF64, 3, 1, 1),     // pc=127: vy^2
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 3),     // pc=128
				vm3.MakeOp(vm3.OpListGetF64, 1, 5, 5), // pc=129: vz
				vm3.MakeOp(vm3.OpMulF64, 3, 1, 1),     // pc=130: vz^2
				vm3.MakeOp(vm3.OpAddF64, 2, 2, 3),     // pc=131: f2 = vsq
				// kin = 0.5 * m[bi] * vsq  (f3)
				vm3.MakeOp(vm3.OpListGetF64, 3, 6, 5), // pc=132: m[bi]
				vm3.MakeOp(vm3.OpConstF64K, 4, 0, 8),  // pc=133: f4 = 0.5
				vm3.MakeOp(vm3.OpMulF64, 3, 3, 4),     // pc=134: 0.5*m
				vm3.MakeOp(vm3.OpMulF64, 3, 3, 2),     // pc=135: kin
				// pot = 0 (f4); bj = bi + 1
				vm3.MakeOp(vm3.OpConstF64K, 4, 0, 0), // pc=136: pot = 0
				vm3.MakeOp(vm3.OpAddI64K, 6, 5, 1),   // pc=137: bj = bi + 1
				// pot_loop pc=138
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 6, 5, 161), // pc=138: if bj>=5 -> pot_done
				// dx (f5 scratch), accumulate squared distance in f5
				vm3.MakeOp(vm3.OpListGetF64, 5, 0, 5), // pc=139: xi
				vm3.MakeOp(vm3.OpListGetF64, 6, 0, 6), // pc=140: xj
				vm3.MakeOp(vm3.OpSubF64, 5, 5, 6),     // pc=141: f5 = dx
				vm3.MakeOp(vm3.OpListGetF64, 6, 1, 5), // pc=142: yi
				vm3.MakeOp(vm3.OpListGetF64, 7, 1, 6), // pc=143: yj
				vm3.MakeOp(vm3.OpSubF64, 6, 6, 7),     // pc=144: f6 = dy
				vm3.MakeOp(vm3.OpMulF64, 5, 5, 5),     // pc=145: dx^2
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 6),     // pc=146: dy^2
				vm3.MakeOp(vm3.OpAddF64, 5, 5, 6),     // pc=147: dx^2+dy^2
				vm3.MakeOp(vm3.OpListGetF64, 6, 2, 5), // pc=148: zi
				vm3.MakeOp(vm3.OpListGetF64, 7, 2, 6), // pc=149: zj
				vm3.MakeOp(vm3.OpSubF64, 6, 6, 7),     // pc=150: f6 = dz
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 6),     // pc=151: dz^2
				vm3.MakeOp(vm3.OpAddF64, 5, 5, 6),     // pc=152: f5 = d2
				vm3.MakeOp(vm3.OpSqrtF64, 5, 5, 0),    // pc=153: f5 = r
				// m[bi] * m[bj] / r ; pot += ...
				vm3.MakeOp(vm3.OpListGetF64, 6, 6, 5), // pc=154: m[bi]
				vm3.MakeOp(vm3.OpListGetF64, 7, 6, 6), // pc=155: m[bj]
				vm3.MakeOp(vm3.OpMulF64, 6, 6, 7),     // pc=156: m[bi]*m[bj]
				vm3.MakeOp(vm3.OpDivF64, 6, 6, 5),     // pc=157: /r
				vm3.MakeOp(vm3.OpAddF64, 4, 4, 6),     // pc=158: pot += ...
				vm3.MakeOp(vm3.OpAddI64K, 6, 6, 1),    // pc=159: bj++
				vm3.MakeOp(vm3.OpJump, 0, 0, 138),     // pc=160 -> pot_loop
				// pot_done pc=161
				vm3.MakeOp(vm3.OpSubF64, 3, 3, 4),  // pc=161: kin - pot
				vm3.MakeOp(vm3.OpAddF64, 0, 0, 3),  // pc=162: e += kin - pot
				vm3.MakeOp(vm3.OpAddI64K, 5, 5, 1), // pc=163: bi++
				vm3.MakeOp(vm3.OpJump, 0, 0, 123),  // pc=164 -> energy_loop
				// end pc=165
				vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0), // pc=165
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
