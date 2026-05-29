// Package target implements TargetErlangPort (MEP-66 Direction 2): given
// a Mochi package's metadata and its translated function surface, it emits
// a self-contained rebar3 application skeleton that wraps the compiled
// Mochi binary as an Erlang OTP Port driver.
//
// Emitted layout:
//
//	<outDir>/
//	  src/
//	    <appName>_mochi_shim.erl   gen_server Port driver (priv/mochi_binary)
//	    <appName>.app.src          OTP application descriptor
//	  rebar.config                  rebar3 + hex publish metadata
//	  priv/                         placeholder; mochi_binary is placed here
//	                                at publish time by `mochi pkg publish`.
//
// The emitted rebar.config includes a `{hex, ...}` metadata block populated
// from the [erlang.publish] table in mochi.toml (HexMeta).
package target

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/package3/erlang/portemit"
)

// HexMeta holds the optional [erlang.publish] fields from mochi.toml.
// Fields are used to populate the hex metadata block in rebar.config.
type HexMeta struct {
	// AppName is the OTP application name (atom). Must be a valid Erlang atom.
	// Defaults to the package Name if empty.
	AppName string
	// Description is the short package description.
	Description string
	// Version is the Hex.pm release version ("1.0.0").
	Version string
	// Licenses is the list of SPDX license identifiers.
	Licenses []string
	// Links is a set of named URLs (e.g. {"GitHub": "https://..."}).
	Links map[string]string
	// Maintainers is a list of email addresses or display names.
	Maintainers []string
	// Files is the list of glob patterns for files to include in the tarball.
	Files []string
	// BuildTools lists the build tools used (default ["rebar3"]).
	BuildTools []string
}

// Opts configures the TargetErlangPort emitter.
type Opts struct {
	// Name is the Mochi package / Hex.pm package name.
	Name string
	// OTPVersion is the minimum OTP version for rebar.config. Default "25".
	OTPVersion string
	// Fns is the list of function specs to expose from the Erlang wrapper.
	// Each spec describes one function in the Port driver gen_server.
	Fns []portemit.FnSpec
	// Meta holds the hex publish metadata. May be zero-valued; defaults
	// are applied for fields that are missing.
	Meta HexMeta
}

// EmitResult records the paths written by Emit.
type EmitResult struct {
	ShimErlPath  string
	AppSrcPath   string
	RebarCfgPath string
}

// Emit writes the rebar3 application skeleton to outDir. outDir is created
// if it does not exist. Returns the paths of the three emitted files.
func Emit(outDir string, opts Opts) (EmitResult, error) {
	if opts.Name == "" {
		return EmitResult{}, fmt.Errorf("target: Name must not be empty")
	}
	appName := resolvedAppName(opts)
	otpVsn := opts.OTPVersion
	if otpVsn == "" {
		otpVsn = "25"
	}

	srcDir := filepath.Join(outDir, "src")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return EmitResult{}, fmt.Errorf("target: create src dir: %w", err)
	}
	if err := os.MkdirAll(filepath.Join(outDir, "priv"), 0o755); err != nil {
		return EmitResult{}, fmt.Errorf("target: create priv dir: %w", err)
	}

	// 1. shim.erl — gen_server Port driver.
	shimContent := portemit.EmitShimErl(appName, opts.Fns)
	shimPath := filepath.Join(srcDir, appName+"_mochi_shim.erl")
	if err := os.WriteFile(shimPath, []byte(shimContent), 0o644); err != nil {
		return EmitResult{}, fmt.Errorf("target: write shim.erl: %w", err)
	}

	// 2. <appName>.app.src — OTP application descriptor.
	appSrc := emitAppSrc(appName, opts.Meta)
	appSrcPath := filepath.Join(srcDir, appName+".app.src")
	if err := os.WriteFile(appSrcPath, []byte(appSrc), 0o644); err != nil {
		return EmitResult{}, fmt.Errorf("target: write .app.src: %w", err)
	}

	// 3. rebar.config — rebar3 + hex metadata.
	rebarCfg := emitRebarConfig(appName, otpVsn, opts.Meta)
	rebarPath := filepath.Join(outDir, "rebar.config")
	if err := os.WriteFile(rebarPath, []byte(rebarCfg), 0o644); err != nil {
		return EmitResult{}, fmt.Errorf("target: write rebar.config: %w", err)
	}

	return EmitResult{
		ShimErlPath:  shimPath,
		AppSrcPath:   appSrcPath,
		RebarCfgPath: rebarPath,
	}, nil
}

func resolvedAppName(opts Opts) string {
	if opts.Meta.AppName != "" {
		return opts.Meta.AppName
	}
	return opts.Name
}

// emitAppSrc generates the OTP .app.src descriptor.
func emitAppSrc(appName string, meta HexMeta) string {
	description := meta.Description
	if description == "" {
		description = appName + " (Mochi Erlang Port driver)"
	}
	version := meta.Version
	if version == "" {
		version = "0.1.0"
	}
	return fmt.Sprintf(`{application, %s,
 [{description, %q},
  {vsn, %q},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {%s_mochi_shim, []}}
 ]}.
`, appName, description, version, appName)
}

// emitRebarConfig generates the rebar.config with hex publish metadata.
func emitRebarConfig(appName, otpVsn string, meta HexMeta) string {
	var sb strings.Builder
	sb.WriteString("%% Auto-generated by mochi pkg publish (MEP-66). Do not edit by hand.\n\n")
	sb.WriteString("{erl_opts, [debug_info]}.\n\n")
	sb.WriteString(fmt.Sprintf("{minimum_otp_vsn, %q}.\n\n", otpVsn))

	// hex metadata block (only when any publish field is set)
	if hasHexMeta(meta) {
		sb.WriteString("{hex, [\n")
		sb.WriteString(fmt.Sprintf("  {name, <<\"%s\">>},\n", appName))
		if meta.Description != "" {
			sb.WriteString(fmt.Sprintf("  {description, <<\"%s\">>},\n", escErl(meta.Description)))
		}
		if meta.Version != "" {
			sb.WriteString(fmt.Sprintf("  {version, <<\"%s\">>},\n", meta.Version))
		}
		if len(meta.Licenses) > 0 {
			sb.WriteString("  {licenses, [")
			for i, l := range meta.Licenses {
				if i > 0 {
					sb.WriteString(", ")
				}
				sb.WriteString(fmt.Sprintf("<<%q>>", l))
			}
			sb.WriteString("]},\n")
		}
		if len(meta.Links) > 0 {
			sb.WriteString("  {links, [")
			first := true
			for label, url := range meta.Links {
				if !first {
					sb.WriteString(", ")
				}
				first = false
				sb.WriteString(fmt.Sprintf("{<<%q>>, <<%q>>}", label, url))
			}
			sb.WriteString("]},\n")
		}
		if len(meta.Maintainers) > 0 {
			sb.WriteString("  {maintainers, [")
			for i, m := range meta.Maintainers {
				if i > 0 {
					sb.WriteString(", ")
				}
				sb.WriteString(fmt.Sprintf("<<%q>>", m))
			}
			sb.WriteString("]},\n")
		}
		buildTools := meta.BuildTools
		if len(buildTools) == 0 {
			buildTools = []string{"rebar3"}
		}
		sb.WriteString("  {build_tools, [")
		for i, bt := range buildTools {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(fmt.Sprintf("<<%q>>", bt))
		}
		sb.WriteString("]}\n")
		sb.WriteString("]}.\n")
	}

	return sb.String()
}

func hasHexMeta(m HexMeta) bool {
	return m.AppName != "" || m.Description != "" || m.Version != "" ||
		len(m.Licenses) > 0 || len(m.Links) > 0 || len(m.Maintainers) > 0
}

// escErl escapes a string for embedding in an Erlang binary literal.
// We only escape double-quotes and backslashes; the output goes inside
// a `<<"...">>` Erlang binary syntax.
func escErl(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, `"`, `\"`)
	return s
}
