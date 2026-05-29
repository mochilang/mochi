// Package nuget provides a NuGet v3 API client and content-addressed
// .nupkg cache for the MEP-68 .NET package bridge.
package nuget

// VersionsIndex is the response from the NuGet v3 flat-container versions
// endpoint:
//
//	GET https://api.nuget.org/v3-flatcontainer/{id}/index.json
type VersionsIndex struct {
	Versions []string `json:"versions"`
}

// ServiceIndex is the NuGet v3 service index returned from
// https://api.nuget.org/v3/index.json. It advertises the URLs of each
// NuGet service resource.
type ServiceIndex struct {
	Version   string            `json:"version"`
	Resources []ServiceResource `json:"resources"`
}

// ServiceResource is one entry in the ServiceIndex resources array.
type ServiceResource struct {
	ID      string `json:"@id"`
	Type    string `json:"@type"`
	Comment string `json:"comment,omitempty"`
}
