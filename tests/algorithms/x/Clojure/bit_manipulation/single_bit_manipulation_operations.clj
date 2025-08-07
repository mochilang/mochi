(ns main (:refer-clojure :exclude [pow2 is_bit_set set_bit clear_bit flip_bit get_bit]))

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

(declare pow2 is_bit_set set_bit clear_bit flip_bit get_bit)

(def ^:dynamic is_bit_set_remainder nil)

(def ^:dynamic is_bit_set_shifted nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_result nil)

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_result nil] (try (do (set! pow2_result 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_result (* pow2_result 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_bit_set [is_bit_set_number is_bit_set_position]
  (binding [is_bit_set_remainder nil is_bit_set_shifted nil] (try (do (set! is_bit_set_shifted (/ is_bit_set_number (pow2 is_bit_set_position))) (set! is_bit_set_remainder (mod is_bit_set_shifted 2)) (throw (ex-info "return" {:v (= is_bit_set_remainder 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_bit [set_bit_number set_bit_position]
  (try (if (is_bit_set set_bit_number set_bit_position) set_bit_number (+ set_bit_number (pow2 set_bit_position))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn clear_bit [clear_bit_number clear_bit_position]
  (try (if (is_bit_set clear_bit_number clear_bit_position) (- clear_bit_number (pow2 clear_bit_position)) clear_bit_number) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn flip_bit [flip_bit_number flip_bit_position]
  (try (if (is_bit_set flip_bit_number flip_bit_position) (- flip_bit_number (pow2 flip_bit_position)) (+ flip_bit_number (pow2 flip_bit_position))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_bit [get_bit_number get_bit_position]
  (try (if (is_bit_set get_bit_number get_bit_position) 1 0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (set_bit 13 1)))
      (println (str (clear_bit 18 1)))
      (println (str (flip_bit 5 1)))
      (println (str (is_bit_set 10 3)))
      (println (str (get_bit 10 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
