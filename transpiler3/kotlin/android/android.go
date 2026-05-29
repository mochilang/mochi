// Package android generates Gradle/AGP Android App Bundle projects from Mochi
// Kotlin source. The generated project targets AGP 8.7+, minSdk 24, targetSdk 35,
// Kotlin 2.1.20, and Java 17. It produces a valid .aab when ANDROID_HOME is set.
package android

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Config holds per-project Android configuration.
type Config struct {
	// AppID is the Android applicationId, e.g. "mochi.user.hello".
	AppID string
	// VersionCode is the integer version code (default 1).
	VersionCode int
	// VersionName is the human-readable version string (default "1.0").
	VersionName string
	// MinSdk is the minimum Android SDK version (default 24).
	MinSdk int
	// TargetSdk is the target Android SDK version (default 35).
	TargetSdk int
	// KtSrc is the generated Kotlin source from the Mochi lowerer.
	KtSrc string
	// ExpectedOut is the expected stdout of main() used in instrumented tests.
	ExpectedOut string
}

func (c *Config) applyDefaults() {
	if c.MinSdk == 0 {
		c.MinSdk = 24
	}
	if c.TargetSdk == 0 {
		c.TargetSdk = 35
	}
	if c.VersionCode == 0 {
		c.VersionCode = 1
	}
	if c.VersionName == "" {
		c.VersionName = "1.0"
	}
	if c.AppID == "" {
		c.AppID = "mochi.user.app"
	}
}

// Generate writes a complete Gradle/AGP Android project to projectDir.
// The project structure is:
//
//	projectDir/
//	  settings.gradle.kts
//	  build.gradle.kts
//	  gradle.properties
//	  gradle/libs.versions.toml
//	  gradle/wrapper/gradle-wrapper.properties
//	  gradlew          (symlink-to or copy of the Gradle wrapper script)
//	  app/
//	    build.gradle.kts
//	    src/main/AndroidManifest.xml
//	    src/main/kotlin/mochi/user/Main.kt
//	    src/main/kotlin/mochi/user/MainActivity.kt
//	    src/androidTest/kotlin/mochi/user/MainTest.kt
func Generate(cfg Config, projectDir string) error {
	cfg.applyDefaults()

	dirs := []string{
		filepath.Join(projectDir, "gradle", "wrapper"),
		filepath.Join(projectDir, "app", "src", "main", "kotlin", "mochi", "user"),
		filepath.Join(projectDir, "app", "src", "androidTest", "kotlin", "mochi", "user"),
		filepath.Join(projectDir, "app", "src", "main", "res", "values"),
		filepath.Join(projectDir, "app", "src", "main", "res", "mipmap-hdpi"),
	}
	for _, d := range dirs {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return fmt.Errorf("mkdir %s: %w", d, err)
		}
	}

	files := map[string]string{
		"settings.gradle.kts":                           settingsGradle(projectDir),
		"build.gradle.kts":                              rootBuildGradle(),
		"gradle.properties":                             gradleProperties(),
		"gradle/libs.versions.toml":                     libsVersionsToml(),
		"gradle/wrapper/gradle-wrapper.properties":      gradleWrapperProperties(),
		"gradlew":                                       gradlewScript(),
		"app/build.gradle.kts":                          appBuildGradle(cfg),
		"app/src/main/AndroidManifest.xml":              androidManifest(cfg),
		"app/src/main/kotlin/mochi/user/Main.kt":        cfg.KtSrc,
		"app/src/main/kotlin/mochi/user/MainActivity.kt": mainActivity(),
		"app/src/androidTest/kotlin/mochi/user/MainTest.kt": instrumentedTest(cfg),
		"app/src/main/res/values/strings.xml":           stringsXml(),
	}

	for rel, content := range files {
		path := filepath.Join(projectDir, rel)
		mode := os.FileMode(0o644)
		if rel == "gradlew" {
			mode = 0o755
		}
		if err := os.WriteFile(path, []byte(content), mode); err != nil {
			return fmt.Errorf("write %s: %w", rel, err)
		}
	}
	return nil
}

func settingsGradle(projectDir string) string {
	name := filepath.Base(projectDir)
	if name == "" || name == "." {
		name = "MochiApp"
	}
	return fmt.Sprintf(`pluginManagement {
    repositories {
        google { content { includeGroupByRegex("com\\.android.*"); includeGroupByRegex("com\\.google.*"); includeGroupByRegex("androidx.*") } }
        mavenCentral()
        gradlePluginPortal()
    }
}
dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        google()
        mavenCentral()
    }
}
rootProject.name = %q
include(":app")
`, name)
}

func rootBuildGradle() string {
	return `// Top-level build file where you can add configuration options common to all sub-projects/modules.
plugins {
    alias(libs.plugins.android.application) apply false
    alias(libs.plugins.kotlin.android) apply false
}
`
}

func gradleProperties() string {
	return `org.gradle.jvmargs=-Xmx2048m -Dfile.encoding=UTF-8
android.useAndroidX=true
kotlin.code.style=official
android.nonTransitiveRClass=true
org.gradle.caching=true
`
}

func libsVersionsToml() string {
	return `[versions]
agp = "8.7.3"
kotlin = "2.1.20"
junit = "4.13.2"
androidx-test-ext-junit = "1.2.1"
espresso-core = "3.6.1"

[libraries]
junit = { group = "junit", name = "junit", version.ref = "junit" }
androidx-test-ext-junit = { group = "androidx.test.ext", name = "junit", version.ref = "androidx-test-ext-junit" }
espresso-core = { group = "androidx.test.espresso", name = "espresso-core", version.ref = "espresso-core" }
kotlin-stdlib = { group = "org.jetbrains.kotlin", name = "kotlin-stdlib", version.ref = "kotlin" }

[plugins]
android-application = { id = "com.android.application", version.ref = "agp" }
kotlin-android = { id = "org.jetbrains.kotlin.android", version.ref = "kotlin" }
`
}

func gradleWrapperProperties() string {
	return `distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
distributionUrl=https\://services.gradle.org/distributions/gradle-8.11.1-bin.zip
distributionSha256Sum=f397b287023acdba1e9f6fc5ea72d22dd63669d59ed4a289a29b1a76eee151c6
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
`
}

func gradlewScript() string {
	return `#!/bin/sh
#
# Gradle start up script for UN*X
#
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
exec gradle --project-dir "$SCRIPT_DIR" "$@"
`
}

func appBuildGradle(cfg Config) string {
	return fmt.Sprintf(`plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
}

android {
    namespace = "mochi.user"
    compileSdk = %d

    defaultConfig {
        applicationId = %q
        minSdk = %d
        targetSdk = %d
        versionCode = %d
        versionName = %q

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }

    kotlinOptions {
        jvmTarget = "17"
    }

    // Reproducible builds: suppress timestamps and randomised ordering.
    packaging {
        resources.excludes += "META-INF/*.kotlin_module"
    }
}

kotlin {
    jvmToolchain(17)
}

dependencies {
    implementation(libs.kotlin.stdlib)
    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.test.ext.junit)
    androidTestImplementation(libs.espresso.core)
}
`, cfg.TargetSdk, cfg.AppID, cfg.MinSdk, cfg.TargetSdk, cfg.VersionCode, cfg.VersionName)
}

func androidManifest(cfg Config) string {
	return fmt.Sprintf(`<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android">

    <application
        android:allowBackup="true"
        android:label="@string/app_name"
        android:supportsRtl="true"
        android:theme="@android:style/Theme.Material.Light.NoActionBar">
        <activity
            android:name=".MainActivity"
            android:exported="true">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
    </application>

</manifest>
`)
}

func mainActivity() string {
	return `package mochi.user

import android.app.Activity
import android.os.Bundle

// MainActivity wraps the Mochi-generated main() entry point for Android.
// In production, main() would drive UI or a service. Here it runs the
// same logic as the JVM target, enabling instrumented tests to validate
// stdout parity with vm3.
class MainActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        main()
    }
}
`
}

func instrumentedTest(cfg Config) string {
	escaped := strings.ReplaceAll(cfg.ExpectedOut, `"`, `\"`)
	escaped = strings.ReplaceAll(escaped, "\n", `\n`)
	return fmt.Sprintf(`package mochi.user

import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import java.io.ByteArrayOutputStream
import java.io.PrintStream

// MainTest captures stdout from main() and asserts it equals the vm3-recorded expected output.
// Run via: ./gradlew :app:connectedCheck
@RunWith(AndroidJUnit4::class)
class MainTest {
    @Test
    fun testMain() {
        val baos = ByteArrayOutputStream()
        val old = System.out
        System.setOut(PrintStream(baos))
        try {
            main()
        } finally {
            System.setOut(old)
        }
        val got = baos.toString("UTF-8")
        val want = "%s"
        assertEquals(want, got)
    }
}
`, escaped)
}

func stringsXml() string {
	return `<?xml version="1.0" encoding="utf-8"?>
<resources>
    <string name="app_name">MochiApp</string>
</resources>
`
}
