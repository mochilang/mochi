package errors

import (
	"strings"
	"testing"
)

func TestSkipReport_Error(t *testing.T) {
	cases := []struct {
		name    string
		report  SkipReport
		wantIn  []string
	}{
		{
			name: "untyped",
			report: SkipReport{
				Gem:     "nokogiri",
				Version: "1.16.2",
				Item:    "Nokogiri::HTML::Document#css",
				Reason:  SkipUntyped,
				RBSType: "untyped",
			},
			wantIn: []string{"Nokogiri::HTML::Document#css", "nokogiri", "untyped"},
		},
		{
			name: "complex union",
			report: SkipReport{
				Gem:     "activesupport",
				Version: "7.1.0",
				Item:    "ActiveSupport::Duration#since",
				Reason:  SkipComplexUnion,
				RBSType: "Time | Date | DateTime",
			},
			wantIn: []string{"ActiveSupport::Duration#since", "activesupport", "complex union"},
		},
		{
			name: "native extension",
			report: SkipReport{
				Gem:     "bcrypt",
				Version: "3.1.20",
				Item:    "BCrypt::Engine.hash_secret",
				Reason:  SkipNativeExtension,
				RBSType: "",
			},
			wantIn: []string{"BCrypt::Engine.hash_secret", "bcrypt", "C extension"},
		},
		{
			name: "platform type",
			report: SkipReport{
				Gem:     "rack",
				Version: "3.0.0",
				Item:    "Rack::IO#read",
				Reason:  SkipPlatformType,
				RBSType: "IO",
			},
			wantIn: []string{"Rack::IO#read", "rack", "platform"},
		},
		{
			name: "self type",
			report: SkipReport{
				Gem:     "builder",
				Version: "3.2.4",
				Item:    "Builder::XmlMarkup#method_missing",
				Reason:  SkipSelfType,
				RBSType: "self",
			},
			wantIn: []string{"Builder::XmlMarkup#method_missing", "builder", "self"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			msg := tc.report.Error()
			if msg == "" {
				t.Error("Error() returned empty string")
			}
			for _, want := range tc.wantIn {
				if !strings.Contains(msg, want) {
					t.Errorf("Error() = %q, expected to contain %q", msg, want)
				}
			}
		})
	}
}

func TestSkipReason_String(t *testing.T) {
	cases := []struct {
		reason SkipReason
		wantIn string
	}{
		{SkipUntyped, "untyped"},
		{SkipTopBot, "top"},
		{SkipSelfType, "self"},
		{SkipComplexUnion, "union"},
		{SkipPlatformType, "platform"},
		{SkipProc, "proc"},
		{SkipNativeExtension, "C extension"},
		{SkipVoidParam, "void"},
		{SkipOpenClass, "open-class"},
		{SkipUnknown, "unknown"},
	}
	for _, tc := range cases {
		t.Run(tc.wantIn, func(t *testing.T) {
			s := tc.reason.String()
			if !strings.Contains(strings.ToLower(s), strings.ToLower(tc.wantIn)) {
				t.Errorf("SkipReason(%d).String() = %q, expected to contain %q", tc.reason, s, tc.wantIn)
			}
		})
	}
}

func TestLockConflict_Error(t *testing.T) {
	cases := []struct {
		name    string
		err     LockConflict
		wantIn  []string
	}{
		{
			name: "basic conflict",
			err: LockConflict{
				GemA:        "rails",
				ConstraintA: ">= 7.0",
				GemB:        "activerecord",
				ConstraintB: "= 6.1.0",
				Message:     "version mismatch",
			},
			wantIn: []string{"rails", ">= 7.0", "activerecord", "= 6.1.0", "version mismatch"},
		},
		{
			name: "transitive conflict",
			err: LockConflict{
				GemA:        "nokogiri",
				ConstraintA: "~> 1.15",
				GemB:        "loofah",
				ConstraintB: ">= 1.16",
				Message:     "incompatible constraints",
			},
			wantIn: []string{"nokogiri", "~> 1.15", "loofah", ">= 1.16"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			msg := tc.err.Error()
			if msg == "" {
				t.Error("Error() returned empty string")
			}
			for _, want := range tc.wantIn {
				if !strings.Contains(msg, want) {
					t.Errorf("LockConflict.Error() = %q, expected to contain %q", msg, want)
				}
			}
		})
	}
}

func TestNativeExtensionSkip_Error(t *testing.T) {
	cases := []struct {
		name    string
		err     NativeExtensionSkip
		wantIn  []string
	}{
		{
			name: "linux platform",
			err: NativeExtensionSkip{
				Gem:      "pg",
				Platform: "arm64-darwin",
				Advice:   "use pg-pure or supply a pre-built binary gem",
			},
			wantIn: []string{"pg", "arm64-darwin", "pre-built binary gem"},
		},
		{
			name: "windows platform",
			err: NativeExtensionSkip{
				Gem:      "bcrypt",
				Platform: "x64-mingw-ucrt",
				Advice:   "use bcrypt-ruby or compile from source",
			},
			wantIn: []string{"bcrypt", "x64-mingw-ucrt"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			msg := tc.err.Error()
			if msg == "" {
				t.Error("Error() returned empty string")
			}
			for _, want := range tc.wantIn {
				if !strings.Contains(msg, want) {
					t.Errorf("NativeExtensionSkip.Error() = %q, expected to contain %q", msg, want)
				}
			}
		})
	}
}
