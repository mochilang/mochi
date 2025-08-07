(ns main (:refer-clojure :exclude [char_value roman_to_int int_to_roman]))

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

(declare char_value roman_to_int int_to_roman)

(def ^:dynamic int_to_roman_factor nil)

(def ^:dynamic int_to_roman_i nil)

(def ^:dynamic int_to_roman_j nil)

(def ^:dynamic int_to_roman_num nil)

(def ^:dynamic int_to_roman_res nil)

(def ^:dynamic int_to_roman_symbol nil)

(def ^:dynamic int_to_roman_value nil)

(def ^:dynamic roman_to_int_i nil)

(def ^:dynamic roman_to_int_total nil)

(def ^:dynamic main_roman_values [1000 900 500 400 100 90 50 40 10 9 5 4 1])

(def ^:dynamic main_roman_symbols ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])

(defn char_value [char_value_c]
  (try (do (when (= char_value_c "I") (throw (ex-info "return" {:v 1}))) (when (= char_value_c "V") (throw (ex-info "return" {:v 5}))) (when (= char_value_c "X") (throw (ex-info "return" {:v 10}))) (when (= char_value_c "L") (throw (ex-info "return" {:v 50}))) (when (= char_value_c "C") (throw (ex-info "return" {:v 100}))) (when (= char_value_c "D") (throw (ex-info "return" {:v 500}))) (if (= char_value_c "M") 1000 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn roman_to_int [roman_to_int_roman]
  (binding [roman_to_int_i nil roman_to_int_total nil] (try (do (set! roman_to_int_total 0) (set! roman_to_int_i 0) (while (< roman_to_int_i (count roman_to_int_roman)) (if (and (< (+ roman_to_int_i 1) (count roman_to_int_roman)) (< (char_value (nth roman_to_int_roman roman_to_int_i)) (char_value (nth roman_to_int_roman (+ roman_to_int_i 1))))) (do (set! roman_to_int_total (- (+ roman_to_int_total (char_value (nth roman_to_int_roman (+ roman_to_int_i 1)))) (char_value (nth roman_to_int_roman roman_to_int_i)))) (set! roman_to_int_i (+ roman_to_int_i 2))) (do (set! roman_to_int_total (+ roman_to_int_total (char_value (nth roman_to_int_roman roman_to_int_i)))) (set! roman_to_int_i (+ roman_to_int_i 1))))) (throw (ex-info "return" {:v roman_to_int_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_to_roman [int_to_roman_number]
  (binding [int_to_roman_factor nil int_to_roman_i nil int_to_roman_j nil int_to_roman_num nil int_to_roman_res nil int_to_roman_symbol nil int_to_roman_value nil] (try (do (set! int_to_roman_num int_to_roman_number) (set! int_to_roman_res "") (set! int_to_roman_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< int_to_roman_i (count main_roman_values))) (do (set! int_to_roman_value (nth main_roman_values int_to_roman_i)) (set! int_to_roman_symbol (nth main_roman_symbols int_to_roman_i)) (set! int_to_roman_factor (/ int_to_roman_num int_to_roman_value)) (set! int_to_roman_num (mod int_to_roman_num int_to_roman_value)) (set! int_to_roman_j 0) (while (< int_to_roman_j int_to_roman_factor) (do (set! int_to_roman_res (str int_to_roman_res int_to_roman_symbol)) (set! int_to_roman_j (+ int_to_roman_j 1)))) (cond (= int_to_roman_num 0) (recur false) :else (do (set! int_to_roman_i (+ int_to_roman_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v int_to_roman_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
