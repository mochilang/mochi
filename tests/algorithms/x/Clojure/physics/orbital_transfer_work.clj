(ns main (:refer-clojure :exclude [pow10 floor format_scientific_3 orbital_transfer_work test_orbital_transfer_work main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 floor format_scientific_3 orbital_transfer_work test_orbital_transfer_work main)

(def ^:dynamic floor_f nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic format_scientific_3_exp nil)

(def ^:dynamic format_scientific_3_exp_abs nil)

(def ^:dynamic format_scientific_3_exp_sign nil)

(def ^:dynamic format_scientific_3_exp_str nil)

(def ^:dynamic format_scientific_3_frac_part nil)

(def ^:dynamic format_scientific_3_frac_str nil)

(def ^:dynamic format_scientific_3_int_part nil)

(def ^:dynamic format_scientific_3_mantissa nil)

(def ^:dynamic format_scientific_3_num nil)

(def ^:dynamic format_scientific_3_scaled nil)

(def ^:dynamic format_scientific_3_sign nil)

(def ^:dynamic format_scientific_3_temp nil)

(def ^:dynamic orbital_transfer_work_G nil)

(def ^:dynamic orbital_transfer_work_work nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (if (>= pow10_n 0) (do (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1))))) (do (set! pow10_i 0) (while (> pow10_i pow10_n) (do (set! pow10_p (/ pow10_p 10.0)) (set! pow10_i (- pow10_i 1)))))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_f nil floor_i nil] (try (do (set! floor_i (long floor_x)) (set! floor_f (double floor_i)) (if (> floor_f floor_x) (double (- floor_i 1)) floor_f)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn format_scientific_3 [format_scientific_3_x]
  (binding [format_scientific_3_exp nil format_scientific_3_exp_abs nil format_scientific_3_exp_sign nil format_scientific_3_exp_str nil format_scientific_3_frac_part nil format_scientific_3_frac_str nil format_scientific_3_int_part nil format_scientific_3_mantissa nil format_scientific_3_num nil format_scientific_3_scaled nil format_scientific_3_sign nil format_scientific_3_temp nil] (try (do (when (= format_scientific_3_x 0.0) (throw (ex-info "return" {:v "0.000e+00"}))) (set! format_scientific_3_sign "") (set! format_scientific_3_num format_scientific_3_x) (when (< format_scientific_3_num 0.0) (do (set! format_scientific_3_sign "-") (set! format_scientific_3_num (- format_scientific_3_num)))) (set! format_scientific_3_exp 0) (while (>= format_scientific_3_num 10.0) (do (set! format_scientific_3_num (/ format_scientific_3_num 10.0)) (set! format_scientific_3_exp (+ format_scientific_3_exp 1)))) (while (< format_scientific_3_num 1.0) (do (set! format_scientific_3_num (* format_scientific_3_num 10.0)) (set! format_scientific_3_exp (- format_scientific_3_exp 1)))) (set! format_scientific_3_temp (floor (+ (* format_scientific_3_num 1000.0) 0.5))) (set! format_scientific_3_scaled (long format_scientific_3_temp)) (when (= format_scientific_3_scaled 10000) (do (set! format_scientific_3_scaled 1000) (set! format_scientific_3_exp (+ format_scientific_3_exp 1)))) (set! format_scientific_3_int_part (/ format_scientific_3_scaled 1000)) (set! format_scientific_3_frac_part (mod format_scientific_3_scaled 1000)) (set! format_scientific_3_frac_str (str format_scientific_3_frac_part)) (while (< (count format_scientific_3_frac_str) 3) (set! format_scientific_3_frac_str (str "0" format_scientific_3_frac_str))) (set! format_scientific_3_mantissa (str (str (str format_scientific_3_int_part) ".") format_scientific_3_frac_str)) (set! format_scientific_3_exp_sign "+") (set! format_scientific_3_exp_abs format_scientific_3_exp) (when (< format_scientific_3_exp 0) (do (set! format_scientific_3_exp_sign "-") (set! format_scientific_3_exp_abs (- format_scientific_3_exp)))) (set! format_scientific_3_exp_str (str format_scientific_3_exp_abs)) (when (< format_scientific_3_exp_abs 10) (set! format_scientific_3_exp_str (str "0" format_scientific_3_exp_str))) (throw (ex-info "return" {:v (str (str (str (str format_scientific_3_sign format_scientific_3_mantissa) "e") format_scientific_3_exp_sign) format_scientific_3_exp_str)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn orbital_transfer_work [orbital_transfer_work_mass_central orbital_transfer_work_mass_object orbital_transfer_work_r_initial orbital_transfer_work_r_final]
  (binding [orbital_transfer_work_G nil orbital_transfer_work_work nil] (try (do (set! orbital_transfer_work_G (* 6.6743 (pow10 (- 11)))) (when (or (<= orbital_transfer_work_r_initial 0.0) (<= orbital_transfer_work_r_final 0.0)) (throw (Exception. "Orbital radii must be greater than zero."))) (set! orbital_transfer_work_work (* (/ (* (* orbital_transfer_work_G orbital_transfer_work_mass_central) orbital_transfer_work_mass_object) 2.0) (- (/ 1.0 orbital_transfer_work_r_initial) (/ 1.0 orbital_transfer_work_r_final)))) (throw (ex-info "return" {:v (format_scientific_3 orbital_transfer_work_work)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_orbital_transfer_work []
  (do (when (not= (orbital_transfer_work (* 5.972 (pow10 24)) 1000.0 (* 6.371 (pow10 6)) (* 7.0 (pow10 6))) "2.811e+09") (throw (Exception. "case1 failed"))) (when (not= (orbital_transfer_work (* 5.972 (pow10 24)) 500.0 (* 7.0 (pow10 6)) (* 6.371 (pow10 6))) "-1.405e+09") (throw (Exception. "case2 failed"))) (when (not= (orbital_transfer_work (* 1.989 (pow10 30)) 1000.0 (* 1.5 (pow10 11)) (* 2.28 (pow10 11))) "1.514e+11") (throw (Exception. "case3 failed")))))

(defn main []
  (do (test_orbital_transfer_work) (println (orbital_transfer_work (* 5.972 (pow10 24)) 1000.0 (* 6.371 (pow10 6)) (* 7.0 (pow10 6))))))

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
