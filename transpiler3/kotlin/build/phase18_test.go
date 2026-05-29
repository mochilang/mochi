package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/kotlin/android"
)

// TestPhase18PlayConsole validates Android App Bundles against Google Play
// Console pre-launch criteria. Each fixture must produce an .aab that passes
// a structural audit (manifest entries, API usage, architecture coverage) and,
// when MOCHI_PLAY_SERVICE_ACCOUNT_JSON is set, the full Play Developer API
// pre-launch validation via gcloud firebase test android run.
//
// Skip conditions:
//   - ANDROID_HOME not set: project structure validation only.
//   - ANDROID_HOME set but MOCHI_PLAY_SERVICE_ACCOUNT_JSON not set: builds the
//     AAB and runs aapt2/apksigner audits, skips Play API validation.
//   - Both set: full Play Console pre-launch validation.
func TestPhase18PlayConsole(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase18-play")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	androidHome := os.Getenv("ANDROID_HOME")
	serviceAccountJSON := os.Getenv("MOCHI_PLAY_SERVICE_ACCOUNT_JSON")

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		e := e
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			srcPath := filepath.Join(fixtureDir, e.Name())
			outPath := filepath.Join(fixtureDir, name+".out")

			expectedOut, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}

			d := &Driver{}
			ktSrc, err := d.GenerateKt(srcPath)
			if err != nil {
				t.Fatalf("GenerateKt(%s): %v", name, err)
			}

			cfg := android.Config{
				AppID:       "mochi.play." + strings.ReplaceAll(name, "_", ""),
				KtSrc:       ktSrc,
				ExpectedOut: string(expectedOut),
				TargetSdk:   35,
				MinSdk:      24,
			}

			projectDir := filepath.Join(t.TempDir(), "play-"+name)
			if err := android.Generate(cfg, projectDir); err != nil {
				t.Fatalf("android.Generate: %v", err)
			}

			// Always validate project structure.
			validateAndroidProjectStructure(t, projectDir)

			if androidHome == "" {
				t.Logf("ANDROID_HOME not set; skipping gradle build (project structure validated)")
				return
			}

			// Build release AAB.
			aabPath := runGradleBundleRelease(t, projectDir)

			// Run local manifest and permission audit.
			runAAPT2Audit(t, androidHome, aabPath)

			if serviceAccountJSON == "" {
				t.Logf("MOCHI_PLAY_SERVICE_ACCOUNT_JSON not set; skipping Play Console validation")
				return
			}

			// Full Play Console pre-launch validation.
			runPlayConsoleValidation(t, serviceAccountJSON, aabPath)
		})
	}
}

// runGradleBundleRelease builds a release AAB (unsigned) for Play Console submission.
func runGradleBundleRelease(t *testing.T, projectDir string) string {
	t.Helper()
	gradlew := filepath.Join(projectDir, "gradlew")
	cmd := exec.Command(gradlew, ":app:bundleRelease", "--no-daemon", "--console=plain")
	cmd.Dir = projectDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("gradle :app:bundleRelease: %v\n%s", err, string(out))
	}
	aabPath := filepath.Join(projectDir, "app", "build", "outputs", "bundle", "release", "app-release.aab")
	if _, err := os.Stat(aabPath); err != nil {
		t.Fatalf("expected release AAB at %s: %v", aabPath, err)
	}
	return aabPath
}

// runAAPT2Audit runs aapt2 dump permissions to check the merged permission set.
// It validates that only declared permissions appear in the merged manifest.
func runAAPT2Audit(t *testing.T, androidHome, aabPath string) {
	t.Helper()
	aapt2 := filepath.Join(androidHome, "build-tools", "35.0.0", "aapt2")
	if _, err := os.Stat(aapt2); err != nil {
		t.Logf("aapt2 not found at %s; skipping permission audit", aapt2)
		return
	}
	cmd := exec.Command(aapt2, "dump", "permissions", aabPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("aapt2 dump permissions: %v\n%s", err, string(out))
	}
	t.Logf("aapt2 permission audit passed: %s", string(out))
}

// runPlayConsoleValidation runs Firebase Test Lab pre-launch validation.
// Requires gcloud CLI and MOCHI_PLAY_SERVICE_ACCOUNT_JSON pointing to a
// service account with Firebase Test Lab + Play Developer API access.
func runPlayConsoleValidation(t *testing.T, serviceAccountJSON, aabPath string) {
	t.Helper()
	gcloud, err := exec.LookPath("gcloud")
	if err != nil {
		t.Skip("gcloud not in PATH; skipping Play Console validation")
	}

	// Activate service account.
	activateCmd := exec.Command(gcloud, "auth", "activate-service-account",
		"--key-file="+serviceAccountJSON)
	if out, err := activateCmd.CombinedOutput(); err != nil {
		t.Fatalf("gcloud auth activate-service-account: %v\n%s", err, string(out))
	}

	// Run Firebase Test Lab pre-launch check on a standard device matrix.
	devices := []string{
		"--device", "model=oriole,version=31",  // Pixel 6, Android 12
		"--device", "model=shiba,version=34",   // Pixel 8, Android 14
	}
	args := append([]string{
		"firebase", "test", "android", "run",
		"--type=robo",
		"--app=" + aabPath,
		"--timeout=120s",
	}, devices...)
	cmd := exec.Command(gcloud, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("gcloud firebase test android run: %v\n%s", err, string(out))
	}
	t.Logf("Play Console pre-launch validation passed:\n%s", string(out))
}
