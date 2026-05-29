# frozen_string_literal: true

# Phase 0 placeholder. Emitted programs use the bare Ruby Kernel `puts` for
# print; later phases (Phase 2+) route formatted output through Mochi::Runtime::IO
# so floats render with Mochi's canonical formatting (one decimal place, etc.)
# rather than Ruby's default.
module Mochi
  module Runtime
    module IO
      module_function

      def putln(value)
        $stdout.puts(format_value(value))
      end

      def format_value(value)
        case value
        when Float
          # Match Mochi canonical: "3" stays as "3" via int promotion, but a
          # genuine 3.0 renders as "3.0", and 1.5 as "1.5".
          formatted = format("%.16g", value)
          formatted.include?(".") ? formatted : "#{formatted}.0"
        when nil
          ""
        else
          value.to_s
        end
      end
    end
  end
end
