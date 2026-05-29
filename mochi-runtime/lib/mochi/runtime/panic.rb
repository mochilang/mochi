# frozen_string_literal: true

module Mochi
  module Runtime
    # Panic is raised by Mochi's `panic(code, msg)` and caught by `try/catch`.
    # The catch binding receives the integer code; the message survives via the
    # standard StandardError#message channel for diagnostics.
    class Panic < StandardError
      attr_reader :code

      def initialize(code, msg)
        super(msg)
        @code = code
      end
    end
  end
end
