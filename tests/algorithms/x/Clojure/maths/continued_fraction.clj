(ns main (:refer-clojure :exclude [floor_div continued_fraction list_to_string]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare floor_div continued_fraction list_to_string)

(def ^:dynamic continued_fraction_den nil)

(def ^:dynamic continued_fraction_integer_part nil)

(def ^:dynamic continued_fraction_num nil)

(def ^:dynamic continued_fraction_result nil)

(def ^:dynamic continued_fraction_tmp nil)

(def ^:dynamic floor_div_q nil)

(def ^:dynamic floor_div_r nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(defn floor_div [floor_div_a floor_div_b]
  (binding [floor_div_q nil floor_div_r nil] (try (do (set! floor_div_q (quot floor_div_a floor_div_b)) (set! floor_div_r (mod floor_div_a floor_div_b)) (when (and (not= floor_div_r 0) (or (and (< floor_div_a 0) (> floor_div_b 0)) (and (> floor_div_a 0) (< floor_div_b 0)))) (set! floor_div_q (- floor_div_q 1))) (throw (ex-info "return" {:v floor_div_q}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn continued_fraction [continued_fraction_numerator continued_fraction_denominator]
  (binding [continued_fraction_den nil continued_fraction_integer_part nil continued_fraction_num nil continued_fraction_result nil continued_fraction_tmp nil] (try (do (set! continued_fraction_num continued_fraction_numerator) (set! continued_fraction_den continued_fraction_denominator) (set! continued_fraction_result []) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! continued_fraction_integer_part (floor_div continued_fraction_num continued_fraction_den)) (set! continued_fraction_result (conj continued_fraction_result continued_fraction_integer_part)) (set! continued_fraction_num (- continued_fraction_num (* continued_fraction_integer_part continued_fraction_den))) (cond (= continued_fraction_num 0) (recur false) :else (do (set! continued_fraction_tmp continued_fraction_num) (set! continued_fraction_num continued_fraction_den) (set! continued_fraction_den continued_fraction_tmp) (recur while_flag_1)))))) (throw (ex-info "return" {:v continued_fraction_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_lst]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_lst)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_lst list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_lst) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Continued Fraction of 0.84375 is: " (list_to_string (continued_fraction 27 32))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
