# frozen_string_literal: true

Gem::Specification.new do |s|
  s.name        = "mochi-runtime"
  s.version     = "0.14.0"
  s.licenses    = ["Apache-2.0"]
  s.summary     = "Runtime support library for the Mochi to Ruby transpiler"
  s.description = "Minimal Ruby helpers that emitted Mochi programs rely on: io, " \
                  "numeric coercions, list/map/set adapters, query, channel agents."
  s.authors     = ["Mochi Authors"]
  s.email       = ["info@mochi.dev"]
  s.homepage    = "https://mochi.dev"
  s.required_ruby_version = ">= 3.2.0"

  s.files = Dir["lib/**/*.rb"] + ["README.md"]
  s.require_paths = ["lib"]
end
