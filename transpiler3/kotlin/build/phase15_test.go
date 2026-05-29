package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/kotlin/android"
)

// TestPhase15AndroidAppBundle verifies that Mochi programs compile to valid
// Android App Bundle projects (AGP 8.7+, minSdk 24, targetSdk 35).
//
// Skip conditions:
//   - ANDROID_HOME not set: generates the project and validates file structure
//     only (no gradle build).
//   - ANDROID_HOME set: runs ./gradlew :app:bundleDebug and validates the AAB
//     with bundletool if BUNDLETOOL_JAR is set.
func TestPhase15AndroidAppBundle(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase15-android")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	androidHome := os.Getenv("ANDROID_HOME")
	bundletoolJar := os.Getenv("BUNDLETOOL_JAR")

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
				AppID:       "mochi.user." + strings.ReplaceAll(name, "_", ""),
				KtSrc:       ktSrc,
				ExpectedOut: string(expectedOut),
			}

			projectDir := filepath.Join(t.TempDir(), "android-"+name)
			if err := android.Generate(cfg, projectDir); err != nil {
				t.Fatalf("android.Generate: %v", err)
			}

			// Validate project structure regardless of ANDROID_HOME.
			validateAndroidProjectStructure(t, projectDir)

			if androidHome == "" {
				t.Logf("ANDROID_HOME not set; skipping gradle build (project structure validated)")
				return
			}

			// Full gradle build.
			aabPath := runGradleBundle(t, projectDir)

			// Validate AAB with bundletool if available.
			if bundletoolJar != "" {
				validateAABWithBundletool(t, bundletoolJar, aabPath)
			} else {
				t.Logf("BUNDLETOOL_JAR not set; skipping bundletool validation (AAB exists at %s)", aabPath)
			}
		})
	}
}

// validateAndroidProjectStructure checks that all required Gradle files exist.
func validateAndroidProjectStructure(t *testing.T, projectDir string) {
	t.Helper()
	required := []string{
		"settings.gradle.kts",
		"build.gradle.kts",
		"gradle.properties",
		"gradle/libs.versions.toml",
		"gradle/wrapper/gradle-wrapper.properties",
		"gradlew",
		"app/build.gradle.kts",
		"app/src/main/AndroidManifest.xml",
		"app/src/main/kotlin/mochi/user/Main.kt",
		"app/src/main/kotlin/mochi/user/MainActivity.kt",
		"app/src/androidTest/kotlin/mochi/user/MainTest.kt",
	}
	for _, rel := range required {
		path := filepath.Join(projectDir, rel)
		if _, err := os.Stat(path); err != nil {
			t.Errorf("missing required Android project file: %s", rel)
		}
	}
}

// runGradleBundle runs ./gradlew :app:bundleDebug and returns the .aab path.
func runGradleBundle(t *testing.T, projectDir string) string {
	t.Helper()
	gradlew := filepath.Join(projectDir, "gradlew")
	cmd := exec.Command(gradlew, ":app:bundleDebug", "--no-daemon", "--console=plain")
	cmd.Dir = projectDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("gradle :app:bundleDebug: %v\n%s", err, string(out))
	}
	aabPath := filepath.Join(projectDir, "app", "build", "outputs", "bundle", "debug", "app-debug.aab")
	if _, err := os.Stat(aabPath); err != nil {
		t.Fatalf("expected AAB at %s: %v", aabPath, err)
	}
	return aabPath
}

// validateAABWithBundletool runs bundletool validate on the AAB.
func validateAABWithBundletool(t *testing.T, bundletoolJar, aabPath string) {
	t.Helper()
	javaPath := resolveJava(t)
	cmd := exec.Command(javaPath, "-jar", bundletoolJar, "validate", "--bundle="+aabPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("bundletool validate %s: %v\n%s", aabPath, err, string(out))
	}
	t.Logf("bundletool validate passed: %s", string(out))
}
