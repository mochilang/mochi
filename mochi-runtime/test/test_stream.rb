# frozen_string_literal: true

require "minitest/autorun"
require_relative "../lib/mochi/runtime/stream"

class TestStream < Minitest::Test
  def test_subscribe_receives_emitted_values_in_order
    s = Mochi::Runtime::Stream.new(8)
    q = s.subscribe
    [1, 2, 3].each { |v| s.emit(v) }
    assert_equal 1, q.pop
    assert_equal 2, q.pop
    assert_equal 3, q.pop
  end

  def test_late_subscriber_misses_prior_values
    s = Mochi::Runtime::Stream.new(8)
    s.emit("dropped")
    late = s.subscribe
    s.emit("seen")
    assert_equal "seen", late.pop
    assert_equal 0, late.size
  end

  def test_multiple_subscribers_each_get_full_stream
    s = Mochi::Runtime::Stream.new(8)
    a = s.subscribe
    b = s.subscribe
    s.emit(:x)
    s.emit(:y)
    assert_equal :x, a.pop
    assert_equal :y, a.pop
    assert_equal :x, b.pop
    assert_equal :y, b.pop
  end

  def test_subscribe_limit_drops_after_threshold
    s = Mochi::Runtime::Stream.new(64)
    q = s.subscribe_limit(2)
    10.times { |i| s.emit(i) }
    drained = []
    drained << q.pop
    drained << q.pop
    assert_equal [0, 1], drained
  end

  def test_limited_queue_pop_blocks_until_push
    lq = Mochi::Runtime::LimitedQueue.new(4)
    t = Thread.new { lq.pop }
    sleep 0.01
    lq.push(:hello)
    assert_equal :hello, t.value
  end

  def test_limited_queue_silently_drops_above_limit
    lq = Mochi::Runtime::LimitedQueue.new(3)
    [10, 20, 30, 40, 50].each { |v| lq.push(v) }
    assert_equal 10, lq.pop
    assert_equal 20, lq.pop
    assert_equal 30, lq.pop
  end
end
