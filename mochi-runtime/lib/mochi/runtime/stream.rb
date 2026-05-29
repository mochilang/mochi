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

      def emit(val)
        @lock.synchronize { @subs.dup }.each { |q| q.push(val) }
      end
    end
  end
end
