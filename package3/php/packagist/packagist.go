// Package packagist implements the Packagist v2 sparse-index client for the
// MEP-75 PHP bridge. It provides HTTP metadata fetch, version resolution,
// and dist URL extraction for Composer packages.
//
// The Packagist v2 API endpoint is:
//
//	GET https://packagist.org/p2/<vendor>/<package>.json
//
// Each response contains an ordered list of version objects with dist URLs,
// dependency constraints, PSR-4 autoload maps, and PHP engine constraints.
//
// See [website/docs/research/0075/04-packagist-ingest.md] for the design.
package packagist
