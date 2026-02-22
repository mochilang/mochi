module mochi

go 1.24.4

require (
	github.com/UserNobody14/tree-sitter-dart v0.0.0-20250228221807-80e23c07b644
	github.com/akrennmair/pascal v0.0.0-20230115195835-14bcf05a6156
	github.com/alecthomas/participle/v2 v2.1.4
	github.com/alexflint/go-arg v1.5.1
	github.com/charmbracelet/fang v0.3.0
	github.com/chzyer/readline v1.5.1
	github.com/fatih/color v1.18.0
	github.com/google/uuid v1.6.0
	github.com/lib/pq v1.10.9
	github.com/marcboeker/go-duckdb v1.8.5
	github.com/mark3labs/mcp-go v0.38.0
	github.com/mattn/go-sqlite3 v1.14.28
	github.com/spf13/cobra v1.9.1
	github.com/stretchr/testify v1.10.0
	github.com/tliron/commonlog v0.2.20
	github.com/tliron/glsp v0.2.2
	github.com/tree-sitter/go-tree-sitter v0.25.0
	github.com/tree-sitter/tree-sitter-c v0.24.1
	github.com/tree-sitter/tree-sitter-c-sharp v0.23.1
	github.com/tree-sitter/tree-sitter-cpp v0.23.4
	github.com/tree-sitter/tree-sitter-fsharp v0.1.0
	github.com/tree-sitter/tree-sitter-go v0.23.4
	github.com/tree-sitter/tree-sitter-haskell v0.23.1
	github.com/tree-sitter/tree-sitter-java v0.23.5
	github.com/tree-sitter/tree-sitter-ocaml v0.24.2
	github.com/tree-sitter/tree-sitter-php v0.24.2
	github.com/tree-sitter/tree-sitter-python v0.23.6
	github.com/tree-sitter/tree-sitter-racket v0.24.7
	github.com/tree-sitter/tree-sitter-ruby v0.23.1
	github.com/tree-sitter/tree-sitter-swift v0.0.0-20250623040733-277b583bbb02
	github.com/tree-sitter/tree-sitter-typescript v0.23.2
	github.com/xwb1989/sqlparser v0.0.0-20180606152119-120387863bf2
	github.com/yuin/gopher-lua v1.1.1
	github.com/z7zmey/php-parser v0.7.2
	golang.org/x/mod v0.27.0
	golang.org/x/tools v0.36.0
	gopkg.in/yaml.v3 v3.0.1
)

replace github.com/tree-sitter/tree-sitter-scheme => github.com/6cdh/tree-sitter-scheme v0.24.7

replace github.com/tree-sitter/tree-sitter-racket => github.com/6cdh/tree-sitter-racket v0.24.7

replace github.com/tree-sitter/tree-sitter-fsharp => github.com/ionide/tree-sitter-fsharp v0.1.0

replace github.com/tree-sitter/tree-sitter-swift => github.com/alex-pinkus/tree-sitter-swift v0.0.0-20250623040733-277b583bbb02

require github.com/tree-sitter/tree-sitter-scala v0.24.0

require (
	github.com/tree-sitter-grammars/tree-sitter-kotlin v1.1.0
	github.com/tree-sitter-grammars/tree-sitter-lua v0.4.0
	github.com/tree-sitter-grammars/tree-sitter-zig v1.1.2
	github.com/tree-sitter/tree-sitter-scheme v0.24.7
)

require (
	github.com/JohannesKaufmann/html-to-markdown v1.6.0
	github.com/PuerkitoBio/goquery v1.10.3
	github.com/tree-sitter/tree-sitter-elixir v0.3.4
	github.com/tree-sitter/tree-sitter-erlang v0.0.0-20250613112728-07dad1469ecb
)

require (
	github.com/andybalholm/cascadia v1.3.3 // indirect
	github.com/dustin/go-humanize v1.0.1 // indirect
	github.com/kballard/go-shellquote v0.0.0-20180428030007-95032a82bc51 // indirect
	github.com/ncruces/go-strftime v0.1.9 // indirect
	github.com/remyoudompheng/bigfft v0.0.0-20230129092748-24d4a6f8daec // indirect
	lukechampine.com/uint128 v1.2.0 // indirect
	modernc.org/cc/v3 v3.41.0 // indirect
	modernc.org/ccgo/v3 v3.16.13 // indirect
	modernc.org/libc v1.29.0 // indirect
	modernc.org/mathutil v1.6.0 // indirect
	modernc.org/memory v1.7.2 // indirect
	modernc.org/opt v0.1.3 // indirect
	modernc.org/strutil v1.2.0 // indirect
	modernc.org/token v1.1.0 // indirect
)

require (
	github.com/alexflint/go-scalar v1.2.0 // indirect
	github.com/apache/arrow-go/v18 v18.4.0 // indirect
	github.com/aymanbagabas/go-osc52/v2 v2.0.1 // indirect
	github.com/bahlo/generic-list-go v0.2.0 // indirect
	github.com/buger/jsonparser v1.1.1 // indirect
	github.com/charmbracelet/colorprofile v0.3.2 // indirect
	github.com/charmbracelet/lipgloss/v2 v2.0.0-beta1 // indirect
	github.com/charmbracelet/x/ansi v0.10.1 // indirect
	github.com/charmbracelet/x/cellbuf v0.0.13 // indirect
	github.com/charmbracelet/x/exp/charmtone v0.0.0-20250821175832-f235fab04313 // indirect
	github.com/charmbracelet/x/exp/color v0.0.0-20250821175832-f235fab04313 // indirect
	github.com/charmbracelet/x/exp/golden v0.0.0-20241011142426-46044092ad91 // indirect
	github.com/charmbracelet/x/term v0.2.1 // indirect
	github.com/davecgh/go-spew v1.1.2-0.20180830191138-d8f796af33cc // indirect
	github.com/go-viper/mapstructure/v2 v2.4.0 // indirect
	github.com/goccy/go-json v0.10.5 // indirect
	github.com/google/flatbuffers v25.2.10+incompatible // indirect
	github.com/gorilla/websocket v1.5.4-0.20250319132907-e064f32e3674 // indirect
	github.com/iancoleman/strcase v0.3.0 // indirect
	github.com/inconshreveable/mousetrap v1.1.0 // indirect
	github.com/invopop/jsonschema v0.13.0 // indirect
	github.com/klauspost/compress v1.18.0 // indirect
	github.com/klauspost/cpuid/v2 v2.3.0 // indirect
	github.com/lucasb-eyer/go-colorful v1.2.0 // indirect
	github.com/mailru/easyjson v0.9.0 // indirect
	github.com/mattn/go-colorable v0.1.14 // indirect
	github.com/mattn/go-isatty v0.0.20 // indirect
	github.com/mattn/go-pointer v0.0.1 // indirect
	github.com/mattn/go-runewidth v0.0.16 // indirect
	github.com/muesli/cancelreader v0.2.2 // indirect
	github.com/muesli/mango v0.2.0 // indirect
	github.com/muesli/mango-cobra v1.2.0 // indirect
	github.com/muesli/mango-pflag v0.1.0 // indirect
	github.com/muesli/roff v0.1.0 // indirect
	github.com/muesli/termenv v0.16.0 // indirect
	github.com/petermattis/goid v0.0.0-20250813065127-a731cc31b4fe // indirect
	github.com/pierrec/lz4/v4 v4.1.22 // indirect
	github.com/pkg/errors v0.9.1 // indirect
	github.com/pmezard/go-difflib v1.0.1-0.20181226105442-5d4384ee4fb2 // indirect
	github.com/rivo/uniseg v0.4.7 // indirect
	github.com/sasha-s/go-deadlock v0.3.5 // indirect
	github.com/segmentio/ksuid v1.0.4 // indirect
	github.com/sourcegraph/jsonrpc2 v0.2.1 // indirect
	github.com/spf13/cast v1.9.2 // indirect
	github.com/spf13/pflag v1.0.7 // indirect
	github.com/tliron/kutil v0.3.27 // indirect
	github.com/tree-sitter/tree-sitter-rust v0.24.0
	github.com/wk8/go-ordered-map/v2 v2.1.8 // indirect
	github.com/xo/terminfo v0.0.0-20220910002029-abceb7e1c41e // indirect
	github.com/yosida95/uritemplate/v3 v3.0.2 // indirect
	github.com/zeebo/xxh3 v1.0.2 // indirect
	golang.org/x/crypto v0.41.0 // indirect
	golang.org/x/exp v0.0.0-20250819193227-8b4c13bb791b // indirect
	golang.org/x/net v0.43.0 // indirect
	golang.org/x/sync v0.16.0 // indirect
	golang.org/x/sys v0.35.0 // indirect
	golang.org/x/term v0.34.0 // indirect
	golang.org/x/text v0.28.0 // indirect
	golang.org/x/xerrors v0.0.0-20240903120638-7835f813f4da // indirect
)

replace github.com/tree-sitter/tree-sitter-elixir => github.com/elixir-lang/tree-sitter-elixir v0.3.4

replace github.com/tree-sitter/tree-sitter-erlang => github.com/WhatsApp/tree-sitter-erlang v0.0.0-20250613112728-07dad1469ecb
