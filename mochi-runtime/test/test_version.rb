# frozen_string_literal: true

require "minitest/autorun"
require_relative "../lib/mochi/runtime/version"

class TestVersion < Minitest::Test
  def test_version_constant_is_defined
    assert defined?(Mochi::Runtime::VERSION),
           "Mochi::Runtime::VERSION must be defined"
  end

  def test_version_is_semver_shaped
    v = Mochi::Runtime::VERSION
    assert_kind_of String, v
    assert_match(/\A\d+\.\d+\.\d+\z/, v,
                 "VERSION #{v.inspect} must match MAJOR.MINOR.PATCH")
  end

  def test_version_is_frozen
    assert Mochi::Runtime::VERSION.frozen?,
           "VERSION should be frozen (frozen_string_literal)"
  end
end
