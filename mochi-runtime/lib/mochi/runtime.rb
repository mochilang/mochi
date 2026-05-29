# frozen_string_literal: true

# Umbrella require for the Mochi Ruby runtime. The emitted programs require
# only this file; sub-modules are pulled in lazily as later phases need them.
require_relative "runtime/version"
require_relative "runtime/io"
require_relative "runtime/stream"

module Mochi
  module Runtime
  end
end
