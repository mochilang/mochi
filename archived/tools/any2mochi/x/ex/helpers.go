//go:build archived

package ex

var skipFuncs = map[string]bool{
	"_index_string": true,
	"_slice_string": true,
	"_count":        true,
	"_sum":          true,
	"_avg":          true,
	"_union":        true,
	"_except":       true,
	"_intersect":    true,
	"_group_by":     true,
	"_query":        true,
}
