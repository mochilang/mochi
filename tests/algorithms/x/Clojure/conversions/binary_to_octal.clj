(ns main (:refer-clojure :exclude [bin_to_octal]))

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

(declare bin_to_octal)

(def ^:dynamic bin_to_octal_b0 nil)

(def ^:dynamic bin_to_octal_b1 nil)

(def ^:dynamic bin_to_octal_b2 nil)

(def ^:dynamic bin_to_octal_c nil)

(def ^:dynamic bin_to_octal_group nil)

(def ^:dynamic bin_to_octal_i nil)

(def ^:dynamic bin_to_octal_index nil)

(def ^:dynamic bin_to_octal_oct_string nil)

(def ^:dynamic bin_to_octal_oct_val nil)

(def ^:dynamic bin_to_octal_padded nil)

(defn bin_to_octal [bin_to_octal_bin_string]
  (binding [bin_to_octal_b0 nil bin_to_octal_b1 nil bin_to_octal_b2 nil bin_to_octal_c nil bin_to_octal_group nil bin_to_octal_i nil bin_to_octal_index nil bin_to_octal_oct_string nil bin_to_octal_oct_val nil bin_to_octal_padded nil] (try (do (set! bin_to_octal_i 0) (while (< bin_to_octal_i (count bin_to_octal_bin_string)) (do (set! bin_to_octal_c (nth bin_to_octal_bin_string bin_to_octal_i)) (when (not (or (= bin_to_octal_c "0") (= bin_to_octal_c "1"))) (throw (Exception. "Non-binary value was passed to the function"))) (set! bin_to_octal_i (+ bin_to_octal_i 1)))) (when (= (count bin_to_octal_bin_string) 0) (throw (Exception. "Empty string was passed to the function"))) (set! bin_to_octal_padded bin_to_octal_bin_string) (while (not= (mod (count bin_to_octal_padded) 3) 0) (set! bin_to_octal_padded (str "0" bin_to_octal_padded))) (set! bin_to_octal_oct_string "") (set! bin_to_octal_index 0) (while (< bin_to_octal_index (count bin_to_octal_padded)) (do (set! bin_to_octal_group (subs bin_to_octal_padded bin_to_octal_index (min (+ bin_to_octal_index 3) (count bin_to_octal_padded)))) (set! bin_to_octal_b0 (if (= (nth bin_to_octal_group 0) "1") 1 0)) (set! bin_to_octal_b1 (if (= (nth bin_to_octal_group 1) "1") 1 0)) (set! bin_to_octal_b2 (if (= (nth bin_to_octal_group 2) "1") 1 0)) (set! bin_to_octal_oct_val (+ (+ (* bin_to_octal_b0 4) (* bin_to_octal_b1 2)) bin_to_octal_b2)) (set! bin_to_octal_oct_string (str bin_to_octal_oct_string (str bin_to_octal_oct_val))) (set! bin_to_octal_index (+ bin_to_octal_index 3)))) (throw (ex-info "return" {:v bin_to_octal_oct_string}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (bin_to_octal "1111"))
      (println (bin_to_octal "101010101010011"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
