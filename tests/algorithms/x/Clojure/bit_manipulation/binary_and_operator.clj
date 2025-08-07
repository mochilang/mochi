(ns main (:refer-clojure :exclude [to_binary zfill binary_and]))

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

(declare to_binary zfill binary_and)

(def ^:dynamic binary_and_a_bin nil)

(def ^:dynamic binary_and_a_pad nil)

(def ^:dynamic binary_and_b_bin nil)

(def ^:dynamic binary_and_b_pad nil)

(def ^:dynamic binary_and_i nil)

(def ^:dynamic binary_and_max_len nil)

(def ^:dynamic binary_and_res nil)

(def ^:dynamic to_binary_bit nil)

(def ^:dynamic to_binary_num nil)

(def ^:dynamic to_binary_res nil)

(def ^:dynamic zfill_pad nil)

(def ^:dynamic zfill_res nil)

(defn to_binary [to_binary_n]
  (binding [to_binary_bit nil to_binary_num nil to_binary_res nil] (try (do (when (= to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! to_binary_num to_binary_n) (set! to_binary_res "") (while (> to_binary_num 0) (do (set! to_binary_bit (mod to_binary_num 2)) (set! to_binary_res (str (str to_binary_bit) to_binary_res)) (set! to_binary_num (quot to_binary_num 2)))) (throw (ex-info "return" {:v to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zfill [zfill_s zfill_width]
  (binding [zfill_pad nil zfill_res nil] (try (do (set! zfill_res zfill_s) (set! zfill_pad (- zfill_width (count zfill_s))) (while (> zfill_pad 0) (do (set! zfill_res (str "0" zfill_res)) (set! zfill_pad (- zfill_pad 1)))) (throw (ex-info "return" {:v zfill_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_and [binary_and_a binary_and_b]
  (binding [binary_and_a_bin nil binary_and_a_pad nil binary_and_b_bin nil binary_and_b_pad nil binary_and_i nil binary_and_max_len nil binary_and_res nil] (try (do (when (or (< binary_and_a 0) (< binary_and_b 0)) (throw (Exception. "the value of both inputs must be positive"))) (set! binary_and_a_bin (to_binary binary_and_a)) (set! binary_and_b_bin (to_binary binary_and_b)) (set! binary_and_max_len (count binary_and_a_bin)) (when (> (count binary_and_b_bin) binary_and_max_len) (set! binary_and_max_len (count binary_and_b_bin))) (set! binary_and_a_pad (zfill binary_and_a_bin binary_and_max_len)) (set! binary_and_b_pad (zfill binary_and_b_bin binary_and_max_len)) (set! binary_and_i 0) (set! binary_and_res "") (while (< binary_and_i binary_and_max_len) (do (if (and (= (nth binary_and_a_pad binary_and_i) "1") (= (nth binary_and_b_pad binary_and_i) "1")) (set! binary_and_res (str binary_and_res "1")) (set! binary_and_res (str binary_and_res "0"))) (set! binary_and_i (+ binary_and_i 1)))) (throw (ex-info "return" {:v (str "0b" binary_and_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (binary_and 25 32))
      (println (binary_and 37 50))
      (println (binary_and 21 30))
      (println (binary_and 58 73))
      (println (binary_and 0 255))
      (println (binary_and 256 256))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
