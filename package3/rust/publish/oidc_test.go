package publish

import (
	"strings"
	"testing"
	"time"
)

func sampleClaims() OIDCClaims {
	return OIDCClaims{
		Issuer:          "https://token.actions.githubusercontent.com",
		Subject:         "repo:mochilang/mochi:ref:refs/heads/main",
		Audience:        Audience,
		Expiry:          time.Now().Add(time.Hour).Unix(),
		IssuedAt:        time.Now().Add(-time.Minute).Unix(),
		Repository:      "mochilang/mochi",
		RepositoryOwner: "mochilang",
		JobWorkflowRef:  "mochilang/mochi/.github/workflows/publish.yml@refs/heads/main",
	}
}

func TestEncodeAndParseOIDCRoundtrip(t *testing.T) {
	original := sampleClaims()
	token := EncodeUnverifiedJWT(original)
	parsed, err := ParseOIDCToken(token)
	if err != nil {
		t.Fatal(err)
	}
	if parsed.Issuer != original.Issuer ||
		parsed.Subject != original.Subject ||
		parsed.Audience != original.Audience ||
		parsed.Expiry != original.Expiry ||
		parsed.Repository != original.Repository {
		t.Errorf("roundtrip mismatch:\nwant %+v\ngot  %+v", original, parsed)
	}
}

func TestParseOIDCTokenRejectsMalformed(t *testing.T) {
	cases := []string{
		"",
		"only-one-part",
		"two.parts",
		"a.b.c.d",
		"!!.!!.!!",
		"a.not_base64!!!.c",
		"a." + base64URL("not json") + ".c",
	}
	for _, c := range cases {
		if _, err := ParseOIDCToken(c); err == nil {
			t.Errorf("expected parse failure for %q", c)
		}
	}
}

func base64URL(s string) string {
	// not real base64; force parse to fail at JSON step
	return "bm90IGpzb24" // "not json"
}

func TestValidateClaimsAcceptsValid(t *testing.T) {
	if err := ValidateClaims(sampleClaims(), time.Now()); err != nil {
		t.Errorf("valid claims rejected: %v", err)
	}
}

func TestValidateClaimsRejectsWrongAudience(t *testing.T) {
	c := sampleClaims()
	c.Audience = "wrong"
	err := ValidateClaims(c, time.Now())
	if err == nil {
		t.Fatal("expected error for wrong audience")
	}
	if !strings.Contains(err.Error(), "audience") {
		t.Errorf("error should mention audience: %v", err)
	}
}

func TestValidateClaimsRejectsExpired(t *testing.T) {
	c := sampleClaims()
	c.Expiry = time.Now().Add(-time.Hour).Unix()
	if err := ValidateClaims(c, time.Now()); err == nil {
		t.Error("expected error for expired token")
	}
}

func TestValidateClaimsRejectsMissingExp(t *testing.T) {
	c := sampleClaims()
	c.Expiry = 0
	if err := ValidateClaims(c, time.Now()); err == nil {
		t.Error("expected error for missing exp")
	}
}

func TestValidateClaimsRejectsMissingIssuer(t *testing.T) {
	c := sampleClaims()
	c.Issuer = ""
	if err := ValidateClaims(c, time.Now()); err == nil {
		t.Error("expected error for missing issuer")
	}
}

func TestValidateClaimsRejectsMissingSubject(t *testing.T) {
	c := sampleClaims()
	c.Subject = ""
	if err := ValidateClaims(c, time.Now()); err == nil {
		t.Error("expected error for missing subject")
	}
}

func TestParseOIDCTokenHandlesAdditionalClaims(t *testing.T) {
	c := sampleClaims()
	c.JobWorkflowRef = "mochilang/mochi/.github/workflows/publish.yml@refs/heads/release-1.0"
	token := EncodeUnverifiedJWT(c)
	parsed, err := ParseOIDCToken(token)
	if err != nil {
		t.Fatal(err)
	}
	if parsed.JobWorkflowRef != c.JobWorkflowRef {
		t.Errorf("JobWorkflowRef lost in roundtrip: got %q want %q", parsed.JobWorkflowRef, c.JobWorkflowRef)
	}
}
