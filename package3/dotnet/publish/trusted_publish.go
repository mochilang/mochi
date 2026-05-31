package publish

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"strings"
	"time"
)

// OIDCMintToken retrieves a GitHub Actions OIDC JWT for the given audience.
// It reads ACTIONS_ID_TOKEN_REQUEST_URL and ACTIONS_ID_TOKEN_REQUEST_TOKEN
// from the environment.
func OIDCMintToken(ctx context.Context, audience string) (string, error) {
	tokenURL := os.Getenv("ACTIONS_ID_TOKEN_REQUEST_URL")
	requestToken := os.Getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
	if tokenURL == "" {
		return "", fmt.Errorf("publish: ACTIONS_ID_TOKEN_REQUEST_URL not set")
	}
	// Append audience parameter.
	if audience != "" {
		sep := "?"
		if strings.Contains(tokenURL, "?") {
			sep = "&"
		}
		tokenURL = tokenURL + sep + "audience=" + audience
	}
	return FetchOIDCToken(ctx, tokenURL, requestToken)
}

// NuGetExchangeToken presents an OIDC JWT to nuget.org's trusted-publishing
// endpoint and returns a short-lived NuGet API key string.
func NuGetExchangeToken(ctx context.Context, jwt, packageName string) (string, error) {
	cfg := OIDCConfig{
		PackageID: packageName,
	}
	tok, err := ExchangeForNuGetToken(ctx, cfg, jwt)
	if err != nil {
		return "", err
	}
	return tok.Token, nil
}

// UploadPackage uploads a .nupkg file to nuget.org using the given API key.
func UploadPackage(ctx context.Context, apiKey, nupkgPath string) error {
	return PublishNupkg(ctx, nuGetUploadEndpoint, apiKey, nupkgPath)
}

// nuGetFlatContainerURL is the NuGet v3 flat container endpoint.
const nuGetFlatContainerURL = "https://api.nuget.org/v3-flatcontainer"

// VerifyPublished polls nuget.org until the given package version appears
// in the flat container index, or until ctx is cancelled.
// Maximum wait: 5 minutes, poll interval: 10 seconds.
func VerifyPublished(ctx context.Context, packageID, version string) error {
	idLower := strings.ToLower(packageID)
	indexURL := fmt.Sprintf("%s/%s/index.json", nuGetFlatContainerURL, idLower)

	deadline := time.Now().Add(5 * time.Minute)
	pollInterval := 10 * time.Second

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}

		found, err := versionExistsInIndex(ctx, indexURL, version)
		if err == nil && found {
			return nil
		}

		if time.Now().After(deadline) {
			return fmt.Errorf("publish: VerifyPublished: timeout waiting for %s@%s to appear on nuget.org", packageID, version)
		}

		timer := time.NewTimer(pollInterval)
		select {
		case <-ctx.Done():
			timer.Stop()
			return ctx.Err()
		case <-timer.C:
		}
	}
}

// versionExistsInIndex checks whether version appears in the NuGet flat container index.
func versionExistsInIndex(ctx context.Context, indexURL, version string) (bool, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, indexURL, nil)
	if err != nil {
		return false, err
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return false, err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return false, nil
	}
	if resp.StatusCode != http.StatusOK {
		return false, fmt.Errorf("index returned %d", resp.StatusCode)
	}
	// Simple string search in the JSON body for the version string.
	// The index is {"versions":["1.0.0","1.0.1",...]}
	buf := make([]byte, 32*1024)
	n, _ := resp.Body.Read(buf)
	body := string(buf[:n])
	return strings.Contains(body, `"`+version+`"`), nil
}
