(ns main (:refer-clojure :exclude [int_to_binary pad_left binary_xor]))

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

(declare int_to_binary pad_left binary_xor)

(def ^:dynamic binary_xor_a_bin nil)

(def ^:dynamic binary_xor_a_pad nil)

(def ^:dynamic binary_xor_b_bin nil)

(def ^:dynamic binary_xor_b_pad nil)

(def ^:dynamic binary_xor_i nil)

(def ^:dynamic binary_xor_max_len nil)

(def ^:dynamic binary_xor_result nil)

(def ^:dynamic int_to_binary_num nil)

(def ^:dynamic int_to_binary_res nil)

(def ^:dynamic pad_left_res nil)

(defn int_to_binary [int_to_binary_n]
  (binding [int_to_binary_num nil int_to_binary_res nil] (try (do (when (= int_to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! int_to_binary_res "") (set! int_to_binary_num int_to_binary_n) (while (> int_to_binary_num 0) (do (set! int_to_binary_res (str (str (mod int_to_binary_num 2)) int_to_binary_res)) (set! int_to_binary_num (quot int_to_binary_num 2)))) (throw (ex-info "return" {:v int_to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad_left [pad_left_s pad_left_width]
  (binding [pad_left_res nil] (try (do (set! pad_left_res pad_left_s) (while (< (count pad_left_res) pad_left_width) (set! pad_left_res (str "0" pad_left_res))) (throw (ex-info "return" {:v pad_left_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_xor [binary_xor_a binary_xor_b]
  (binding [binary_xor_a_bin nil binary_xor_a_pad nil binary_xor_b_bin nil binary_xor_b_pad nil binary_xor_i nil binary_xor_max_len nil binary_xor_result nil] (try (do (when (or (< binary_xor_a 0) (< binary_xor_b 0)) (throw (Exception. "the value of both inputs must be positive"))) (set! binary_xor_a_bin (int_to_binary binary_xor_a)) (set! binary_xor_b_bin (int_to_binary binary_xor_b)) (set! binary_xor_max_len (if (> (count binary_xor_a_bin) (count binary_xor_b_bin)) (count binary_xor_a_bin) (count binary_xor_b_bin))) (set! binary_xor_a_pad (pad_left binary_xor_a_bin binary_xor_max_len)) (set! binary_xor_b_pad (pad_left binary_xor_b_bin binary_xor_max_len)) (set! binary_xor_i 0) (set! binary_xor_result "") (while (< binary_xor_i binary_xor_max_len) (do (if (not= (nth binary_xor_a_pad binary_xor_i) (nth binary_xor_b_pad binary_xor_i)) (set! binary_xor_result (str binary_xor_result "1")) (set! binary_xor_result (str binary_xor_result "0"))) (set! binary_xor_i (+ binary_xor_i 1)))) (throw (ex-info "return" {:v (str "0b" binary_xor_result)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (binary_xor 25 32))
      (println (binary_xor 37 50))
      (println (binary_xor 21 30))
      (println (binary_xor 58 73))
      (println (binary_xor 0 255))
      (println (binary_xor 256 256))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
