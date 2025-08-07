(ns main (:refer-clojure :exclude [pad_left_num to_binary show_bits lshift rshift swap_odd_even_bits main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pad_left_num to_binary show_bits lshift rshift swap_odd_even_bits main)

(def ^:dynamic lshift_i nil)

(def ^:dynamic lshift_result nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_nums nil)

(def ^:dynamic pad_left_num_s nil)

(def ^:dynamic rshift_i nil)

(def ^:dynamic rshift_result nil)

(def ^:dynamic swap_odd_even_bits_bit1 nil)

(def ^:dynamic swap_odd_even_bits_bit2 nil)

(def ^:dynamic swap_odd_even_bits_i nil)

(def ^:dynamic swap_odd_even_bits_n nil)

(def ^:dynamic swap_odd_even_bits_result nil)

(def ^:dynamic to_binary_bits nil)

(def ^:dynamic to_binary_min_width nil)

(def ^:dynamic to_binary_num nil)

(def ^:dynamic to_binary_sign nil)

(defn pad_left_num [pad_left_num_n]
  (binding [pad_left_num_s nil] (try (do (set! pad_left_num_s (str pad_left_num_n)) (while (< (count pad_left_num_s) 5) (set! pad_left_num_s (str " " pad_left_num_s))) (throw (ex-info "return" {:v pad_left_num_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_binary [to_binary_n]
  (binding [to_binary_bits nil to_binary_min_width nil to_binary_num nil to_binary_sign nil] (try (do (set! to_binary_sign "") (set! to_binary_num to_binary_n) (when (< to_binary_num 0) (do (set! to_binary_sign "-") (set! to_binary_num (- 0 to_binary_num)))) (set! to_binary_bits "") (while (> to_binary_num 0) (do (set! to_binary_bits (str (str (mod to_binary_num 2)) to_binary_bits)) (set! to_binary_num (quot (- to_binary_num (mod to_binary_num 2)) 2)))) (when (= to_binary_bits "") (set! to_binary_bits "0")) (set! to_binary_min_width 8) (while (< (count to_binary_bits) (- to_binary_min_width (count to_binary_sign))) (set! to_binary_bits (str "0" to_binary_bits))) (throw (ex-info "return" {:v (str to_binary_sign to_binary_bits)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn show_bits [show_bits_before show_bits_after]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str (pad_left_num show_bits_before) ": ") (to_binary show_bits_before)) "\n") (pad_left_num show_bits_after)) ": ") (to_binary show_bits_after))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lshift [lshift_num lshift_k]
  (binding [lshift_i nil lshift_result nil] (try (do (set! lshift_result lshift_num) (set! lshift_i 0) (while (< lshift_i lshift_k) (do (set! lshift_result (* lshift_result 2)) (set! lshift_i (+ lshift_i 1)))) (throw (ex-info "return" {:v lshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rshift [rshift_num rshift_k]
  (binding [rshift_i nil rshift_result nil] (try (do (set! rshift_result rshift_num) (set! rshift_i 0) (while (< rshift_i rshift_k) (do (set! rshift_result (quot (- rshift_result (mod rshift_result 2)) 2)) (set! rshift_i (+ rshift_i 1)))) (throw (ex-info "return" {:v rshift_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn swap_odd_even_bits [swap_odd_even_bits_num]
  (binding [swap_odd_even_bits_bit1 nil swap_odd_even_bits_bit2 nil swap_odd_even_bits_i nil swap_odd_even_bits_n nil swap_odd_even_bits_result nil] (try (do (set! swap_odd_even_bits_n swap_odd_even_bits_num) (when (< swap_odd_even_bits_n 0) (set! swap_odd_even_bits_n (+ swap_odd_even_bits_n 4294967296))) (set! swap_odd_even_bits_result 0) (set! swap_odd_even_bits_i 0) (while (< swap_odd_even_bits_i 32) (do (set! swap_odd_even_bits_bit1 (mod (rshift swap_odd_even_bits_n swap_odd_even_bits_i) 2)) (set! swap_odd_even_bits_bit2 (mod (rshift swap_odd_even_bits_n (+ swap_odd_even_bits_i 1)) 2)) (set! swap_odd_even_bits_result (+ (+ swap_odd_even_bits_result (lshift swap_odd_even_bits_bit1 (+ swap_odd_even_bits_i 1))) (lshift swap_odd_even_bits_bit2 swap_odd_even_bits_i))) (set! swap_odd_even_bits_i (+ swap_odd_even_bits_i 2)))) (throw (ex-info "return" {:v swap_odd_even_bits_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_i nil main_n nil main_nums nil] (do (set! main_nums [(- 1) 0 1 2 3 4 23 24]) (set! main_i 0) (while (< main_i (count main_nums)) (do (set! main_n (nth main_nums main_i)) (println (show_bits main_n (swap_odd_even_bits main_n))) (println "") (set! main_i (+ main_i 1)))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
