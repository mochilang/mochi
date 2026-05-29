package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase10Streams walks the Phase 10 stream fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the end-to-end
// gate.
func TestPhase10Streams(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase10-streams")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runPhpFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

// TestPhase10EmitFragments asserts that the PHP lowerer emits the
// expected MochiStream / MochiSub shape and helper calls for each
// stream feature: make_stream, subscribe, emit, recv_sub, multi-sub
// fan-out, multi-stream programs, and loop-driven emit/recv pairs.
//
// Streams lower to two inline runtime classes plus five helper
// functions; user code only ever sees `mochi_stream_make`,
// `mochi_sub_make`, `mochi_stream_emit`, `mochi_sub_recv` (and
// `mochi_sub_make_limit` for Phase 10.2 backpressure).
func TestPhase10EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "stream_int.mochi",
			wants: []string{
				// Inline runtime classes ship with every stream-using
				// program; they're idempotent across fixtures.
				`final class MochiStream`,
				`final class MochiSub`,
				`public array $subs = [];`,
				`public array $limits = [];`,
				// Public constructor parameter promotion gives us
				// `public int $cap` for free.
				`public int $cap`,
				// Helpers are five named functions; the runtime never
				// branches on element type at PHP-source level.
				`function mochi_stream_make(int $cap): MochiStream`,
				`function mochi_sub_make(MochiStream $s): MochiSub`,
				`function mochi_stream_emit(MochiStream $s, $v): void`,
				`function mochi_sub_recv(MochiSub $sub): mixed`,
				// User-level lowering: bare builtin calls.
				`$s = mochi_stream_make(4);`,
				`$sub = mochi_sub_make($s);`,
				`mochi_stream_emit($s, 10);`,
				`$a = mochi_sub_recv($sub);`,
			},
		},
		{
			fixture: "stream_string.mochi",
			wants: []string{
				// Element types don't affect the helper signatures; the
				// PHP type `mixed` on the emit/recv path covers all of
				// int/float/bool/string uniformly.
				`mochi_stream_emit($s, "hello");`,
				`mochi_stream_emit($s, "world");`,
				`$a = mochi_sub_recv($sub);`,
			},
		},
		{
			fixture: "stream_bool.mochi",
			wants: []string{
				`mochi_stream_emit($s, true);`,
				`mochi_stream_emit($s, false);`,
			},
		},
		{
			fixture: "stream_float.mochi",
			wants: []string{
				// 1.5 ends up as a float literal in PHP source. Newer
				// PHP releases also accept `1.5`.
				`mochi_stream_emit($s, 1.5);`,
				`mochi_stream_emit($s, 2.5);`,
			},
		},
		{
			fixture: "stream_multi_sub.mochi",
			wants: []string{
				// Two distinct subscribers on one stream: each gets its
				// own queue slot inside $s->subs.
				`$sub1 = mochi_sub_make($s);`,
				`$sub2 = mochi_sub_make($s);`,
				// Each recv on a different sub returns from that sub's
				// own queue head.
				`$a1 = mochi_sub_recv($sub1);`,
				`$b1 = mochi_sub_recv($sub2);`,
			},
		},
		{
			fixture: "stream_chain.mochi",
			wants: []string{
				// Two independent streams in one program: each carries
				// its own MochiStream instance and subscriber set.
				`$s1 = mochi_stream_make(3);`,
				`$s2 = mochi_stream_make(3);`,
				`$sub1 = mochi_sub_make($s1);`,
				`$sub2 = mochi_sub_make($s2);`,
				`mochi_stream_emit($s1, 10);`,
				`mochi_stream_emit($s2, 20);`,
			},
		},
		{
			fixture: "stream_loop.mochi",
			wants: []string{
				// Inside a while loop, emit and recv lower to plain
				// helper calls. The loop body holds no scheduler hooks
				// since all Phase 10 fixtures are emit-before-recv.
				`while`,
				`mochi_stream_emit($s, $i);`,
				`$v = mochi_sub_recv($sub);`,
			},
		},
		{
			fixture: "stream_large.mochi",
			wants: []string{
				// Capacity 10 with 10 emits before 10 recvs: still emit-
				// before-recv pattern, just at the buffer's edge.
				`$s = mochi_stream_make(10);`,
				`mochi_stream_emit($s, ($i * $i));`,
			},
		},
		{
			fixture: "stream_accumulate.mochi",
			wants: []string{
				// total += recv_sub(sub) lowers each receive as its own
				// expression inside the binary add.
				`$total = ($total + mochi_sub_recv($sub));`,
			},
		},
		{
			fixture: "stream_emit_after_sub.mochi",
			wants: []string{
				// Subscriber created before emit: the subscription
				// captures every subsequent emit, mirroring the
				// pub/sub contract.
				`$sub = mochi_sub_make($s);`,
				`mochi_stream_emit($s, 42);`,
				`$v = mochi_sub_recv($sub);`,
			},
		},
		{
			fixture: "stream_backpressure.mochi",
			wants: []string{
				// Phase 10.2 helper must be emitted alongside the
				// default subscribe path so the drop branch exists.
				`function mochi_sub_make_limit(MochiStream $s, int $limit): MochiSub`,
				`$s->limits[$idx] = $limit;`,
				// mochi_stream_emit's per-subscriber drop check: if the
				// queue length has hit the configured limit, the new
				// value is discarded silently.
				`if ($s->limits[$k] > 0 && count($s->subs[$k]) >= $s->limits[$k]) { continue; }`,
				// User call sites: subscribe_limit lowers to the
				// dedicated helper, not mochi_sub_make.
				`$sub = mochi_sub_make_limit($s, 2);`,
				`mochi_stream_emit($s, 5);`,
				`mochi_stream_emit($s, 99);`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase10-streams", c.fixture)
			if _, err := os.Stat(mochiPath); err != nil {
				t.Skipf("fixture missing: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(mochiPath, outDir, TargetPhpSource)
			if err != nil {
				t.Fatalf("Build(%s): %v", c.fixture, err)
			}
			data, err := os.ReadFile(p)
			if err != nil {
				t.Fatalf("read %s: %v", p, err)
			}
			src := string(data)
			for _, want := range c.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s: emitted source missing %q\n---\n%s", c.fixture, want, src)
				}
			}
		})
	}
}
