# frozen_string_literal: true

module Mochi
  module Runtime
    # Stream is a bounded MPMC broadcast channel. Each subscriber gets its own
    # SizedQueue and sees every value emitted after it subscribed. Emit blocks
    # only when the slowest live subscriber's queue is full.
    class Stream
      def initialize(cap)
        @cap = cap
        @subs = []
        @lock = Mutex.new
      end

      def subscribe
        q = Thread::SizedQueue.new(@cap)
        @lock.synchronize { @subs << q }
        q
      end

      # subscribe_limit returns a subscriber whose queue silently drops new
      # values once it already holds `limit` items. Used by Mochi's
      # subscribe_limit(stream, N) form for back-pressure-tolerant consumers.
      def subscribe_limit(limit)
        q = LimitedQueue.new(limit)
        @lock.synchronize { @subs << q }
        q
      end

      def emit(val)
        @lock.synchronize { @subs.dup }.each { |q| q.push(val) }
      end
    end

    # LimitedQueue wraps Thread::Queue and drops on push when size >= limit.
    # Pop blocks normally on the underlying queue.
    class LimitedQueue
      def initialize(limit)
        @limit = limit
        @q = Thread::Queue.new
        @lock = Mutex.new
      end

      def push(val)
        @lock.synchronize do
          @q.push(val) if @q.size < @limit
        end
      end

      def pop
        @q.pop
      end
    end
  end
end
