# frozen_string_literal: true

require "minitest/autorun"
require_relative "../lib/mochi/runtime/panic"

class TestPanic < Minitest::Test
  def test_panic_is_standard_error
    assert_kind_of StandardError, Mochi::Runtime::Panic.new(0, "x")
  end

  def test_panic_carries_code_and_message
    p = Mochi::Runtime::Panic.new(7, "boom")
    assert_equal 7, p.code
    assert_equal "boom", p.message
  end

  def test_panic_round_trip_through_rescue
    code = nil
    msg = nil
    begin
      raise Mochi::Runtime::Panic.new(42, "answer")
    rescue Mochi::Runtime::Panic => e
      code = e.code
      msg = e.message
    end
    assert_equal 42, code
    assert_equal "answer", msg
  end

  def test_panic_does_not_rescue_other_errors
    refute_kind_of Mochi::Runtime::Panic, RuntimeError.new("nope")
  end

  def test_panic_code_is_read_only
    p = Mochi::Runtime::Panic.new(1, "")
    refute_respond_to p, :code=
  end
end
