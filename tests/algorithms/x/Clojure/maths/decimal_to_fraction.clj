(ns main (:refer-clojure :exclude [pow10 gcd parse_decimal reduce decimal_to_fraction_str decimal_to_fraction assert_fraction test_decimal_to_fraction main]))

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

(declare pow10 gcd parse_decimal reduce decimal_to_fraction_str decimal_to_fraction assert_fraction test_decimal_to_fraction main)

(def ^:dynamic first_v nil)

(def ^:dynamic gcd_r nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic main_fr nil)

(def ^:dynamic parse_decimal_c nil)

(def ^:dynamic parse_decimal_denominator nil)

(def ^:dynamic parse_decimal_exp nil)

(def ^:dynamic parse_decimal_exp_sign nil)

(def ^:dynamic parse_decimal_exp_str nil)

(def ^:dynamic parse_decimal_frac_part nil)

(def ^:dynamic parse_decimal_idx nil)

(def ^:dynamic parse_decimal_int_part nil)

(def ^:dynamic parse_decimal_num_str nil)

(def ^:dynamic parse_decimal_numerator nil)

(def ^:dynamic parse_decimal_sign nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic reduce_g nil)

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_result (* pow10_result 10)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gcd [gcd_a gcd_b]
  (binding [gcd_r nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (when (< gcd_x 0) (set! gcd_x (- gcd_x))) (when (< gcd_y 0) (set! gcd_y (- gcd_y))) (while (not= gcd_y 0) (do (set! gcd_r (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_r))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_decimal [parse_decimal_s]
  (binding [first_v nil parse_decimal_c nil parse_decimal_denominator nil parse_decimal_exp nil parse_decimal_exp_sign nil parse_decimal_exp_str nil parse_decimal_frac_part nil parse_decimal_idx nil parse_decimal_int_part nil parse_decimal_num_str nil parse_decimal_numerator nil parse_decimal_sign nil] (try (do (when (= (count parse_decimal_s) 0) (throw (Exception. "invalid number"))) (set! parse_decimal_idx 0) (set! parse_decimal_sign 1) (set! first_v (subs parse_decimal_s 0 (min 1 (count parse_decimal_s)))) (if (= first_v "-") (do (set! parse_decimal_sign (- 1)) (set! parse_decimal_idx 1)) (when (= first_v "+") (set! parse_decimal_idx 1))) (set! parse_decimal_int_part "") (loop [while_flag_1 true] (when (and while_flag_1 (< parse_decimal_idx (count parse_decimal_s))) (do (set! parse_decimal_c (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s)))) (if (and (>= (compare parse_decimal_c "0") 0) (<= (compare parse_decimal_c "9") 0)) (do (set! parse_decimal_int_part (str parse_decimal_int_part parse_decimal_c)) (set! parse_decimal_idx (+ parse_decimal_idx 1)) (recur while_flag_1)) (recur false))))) (set! parse_decimal_frac_part "") (when (and (< parse_decimal_idx (count parse_decimal_s)) (= (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s))) ".")) (do (set! parse_decimal_idx (+ parse_decimal_idx 1)) (loop [while_flag_2 true] (when (and while_flag_2 (< parse_decimal_idx (count parse_decimal_s))) (do (set! parse_decimal_c (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s)))) (if (and (>= (compare parse_decimal_c "0") 0) (<= (compare parse_decimal_c "9") 0)) (do (set! parse_decimal_frac_part (str parse_decimal_frac_part parse_decimal_c)) (set! parse_decimal_idx (+ parse_decimal_idx 1)) (recur while_flag_2)) (recur false))))))) (set! parse_decimal_exp 0) (when (and (< parse_decimal_idx (count parse_decimal_s)) (or (= (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s))) "e") (= (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s))) "E"))) (do (set! parse_decimal_idx (+ parse_decimal_idx 1)) (set! parse_decimal_exp_sign 1) (if (and (< parse_decimal_idx (count parse_decimal_s)) (= (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s))) "-")) (do (set! parse_decimal_exp_sign (- 1)) (set! parse_decimal_idx (+ parse_decimal_idx 1))) (when (and (< parse_decimal_idx (count parse_decimal_s)) (= (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s))) "+")) (set! parse_decimal_idx (+ parse_decimal_idx 1)))) (set! parse_decimal_exp_str "") (while (< parse_decimal_idx (count parse_decimal_s)) (do (set! parse_decimal_c (subs parse_decimal_s parse_decimal_idx (min (+ parse_decimal_idx 1) (count parse_decimal_s)))) (if (and (>= (compare parse_decimal_c "0") 0) (<= (compare parse_decimal_c "9") 0)) (do (set! parse_decimal_exp_str (str parse_decimal_exp_str parse_decimal_c)) (set! parse_decimal_idx (+ parse_decimal_idx 1))) (throw (Exception. "invalid number"))))) (when (= (count parse_decimal_exp_str) 0) (throw (Exception. "invalid number"))) (set! parse_decimal_exp (* parse_decimal_exp_sign (int parse_decimal_exp_str))))) (when (not= parse_decimal_idx (count parse_decimal_s)) (throw (Exception. "invalid number"))) (when (= (count parse_decimal_int_part) 0) (set! parse_decimal_int_part "0")) (set! parse_decimal_num_str (str parse_decimal_int_part parse_decimal_frac_part)) (set! parse_decimal_numerator (int parse_decimal_num_str)) (when (= parse_decimal_sign (- 0 1)) (set! parse_decimal_numerator (- 0 parse_decimal_numerator))) (set! parse_decimal_denominator (pow10 (count parse_decimal_frac_part))) (if (> parse_decimal_exp 0) (set! parse_decimal_numerator (* parse_decimal_numerator (pow10 parse_decimal_exp))) (when (< parse_decimal_exp 0) (set! parse_decimal_denominator (* parse_decimal_denominator (pow10 (- parse_decimal_exp)))))) (throw (ex-info "return" {:v {:denominator parse_decimal_denominator :numerator parse_decimal_numerator}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reduce [reduce_fr]
  (binding [reduce_g nil] (try (do (set! reduce_g (gcd (:numerator reduce_fr) (:denominator reduce_fr))) (throw (ex-info "return" {:v {:denominator (quot (:denominator reduce_fr) reduce_g) :numerator (quot (:numerator reduce_fr) reduce_g)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_to_fraction_str [decimal_to_fraction_str_s]
  (try (throw (ex-info "return" {:v (reduce (parse_decimal decimal_to_fraction_str_s))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn decimal_to_fraction [decimal_to_fraction_x]
  (try (throw (ex-info "return" {:v (decimal_to_fraction_str (str decimal_to_fraction_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn assert_fraction [assert_fraction_name assert_fraction_fr assert_fraction_num assert_fraction_den]
  (do (when (or (not= (:numerator assert_fraction_fr) assert_fraction_num) (not= (:denominator assert_fraction_fr) assert_fraction_den)) (throw (Exception. assert_fraction_name))) assert_fraction_name))

(defn test_decimal_to_fraction []
  (do (assert_fraction "case1" (decimal_to_fraction 2.0) 2 1) (assert_fraction "case2" (decimal_to_fraction 89.0) 89 1) (assert_fraction "case3" (decimal_to_fraction_str "67") 67 1) (assert_fraction "case4" (decimal_to_fraction_str "45.0") 45 1) (assert_fraction "case5" (decimal_to_fraction 1.5) 3 2) (assert_fraction "case6" (decimal_to_fraction_str "6.25") 25 4) (assert_fraction "case7" (decimal_to_fraction 0.0) 0 1) (assert_fraction "case8" (decimal_to_fraction (- 2.5)) (- 5) 2) (assert_fraction "case9" (decimal_to_fraction 0.125) 1 8) (assert_fraction "case10" (decimal_to_fraction 1000000.25) 4000001 4) (assert_fraction "case11" (decimal_to_fraction 1.3333) 13333 10000) (assert_fraction "case12" (decimal_to_fraction_str "1.23e2") 123 1) (assert_fraction "case13" (decimal_to_fraction_str "0.500") 1 2)))

(defn main []
  (binding [main_fr nil] (do (test_decimal_to_fraction) (set! main_fr (decimal_to_fraction 1.5)) (println (str (str (str (:numerator main_fr)) "/") (str (:denominator main_fr)))))))

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
