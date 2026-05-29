# frozen_string_literal: true

require "minitest/autorun"
require "stringio"
require_relative "../lib/mochi/runtime/io"

class TestIO < Minitest::Test
  def test_format_int
    assert_equal "0", Mochi::Runtime::IO.format_value(0)
    assert_equal "42", Mochi::Runtime::IO.format_value(42)
    assert_equal "-7", Mochi::Runtime::IO.format_value(-7)
  end

  def test_format_float_integer_valued_gets_dot_zero
    assert_equal "0.0", Mochi::Runtime::IO.format_value(0.0)
    assert_equal "3.0", Mochi::Runtime::IO.format_value(3.0)
    assert_equal "-7.0", Mochi::Runtime::IO.format_value(-7.0)
  end

  def test_format_float_fractional
    assert_equal "1.5", Mochi::Runtime::IO.format_value(1.5)
    assert_equal "3.14", Mochi::Runtime::IO.format_value(3.14)
    assert_equal "-2.5", Mochi::Runtime::IO.format_value(-2.5)
  end

  def test_format_nil_renders_empty
    assert_equal "", Mochi::Runtime::IO.format_value(nil)
  end

  def test_format_bool
    assert_equal "true", Mochi::Runtime::IO.format_value(true)
    assert_equal "false", Mochi::Runtime::IO.format_value(false)
  end

  def test_format_string_round_trips
    assert_equal "hello", Mochi::Runtime::IO.format_value("hello")
    assert_equal "", Mochi::Runtime::IO.format_value("")
    assert_equal "héllo", Mochi::Runtime::IO.format_value("héllo")
  end

  def test_putln_writes_to_stdout_with_newline
    original = $stdout
    captured = StringIO.new
    $stdout = captured
    Mochi::Runtime::IO.putln(42)
    Mochi::Runtime::IO.putln(3.0)
    Mochi::Runtime::IO.putln("hi")
    assert_equal "42\n3.0\nhi\n", captured.string
  ensure
    $stdout = original
  end
end
