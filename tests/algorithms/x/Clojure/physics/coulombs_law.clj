(ns main (:refer-clojure :exclude [format2 coulombs_law]))

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

(declare format2 coulombs_law)

(def ^:dynamic coulombs_law_force nil)

(def ^:dynamic format2_frac_part nil)

(def ^:dynamic format2_frac_str nil)

(def ^:dynamic format2_i nil)

(def ^:dynamic format2_int_part nil)

(def ^:dynamic format2_m nil)

(def ^:dynamic format2_scaled nil)

(def ^:dynamic format2_sign nil)

(def ^:dynamic format2_y nil)

(defn format2 [format2_x]
  (binding [format2_frac_part nil format2_frac_str nil format2_i nil format2_int_part nil format2_m nil format2_scaled nil format2_sign nil format2_y nil] (try (do (set! format2_sign (if (< format2_x 0.0) "-" "")) (set! format2_y (if (< format2_x 0.0) (- format2_x) format2_x)) (set! format2_m 100.0) (set! format2_scaled (* format2_y format2_m)) (set! format2_i (long format2_scaled)) (when (>= (- format2_scaled (double format2_i)) 0.5) (set! format2_i (+ format2_i 1))) (set! format2_int_part (/ format2_i 100)) (set! format2_frac_part (mod format2_i 100)) (set! format2_frac_str (str format2_frac_part)) (when (< format2_frac_part 10) (set! format2_frac_str (str "0" format2_frac_str))) (throw (ex-info "return" {:v (str (str (str format2_sign (str format2_int_part)) ".") format2_frac_str)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_K 8987551792.3)

(defn coulombs_law [coulombs_law_q1 coulombs_law_q2 coulombs_law_radius]
  (binding [coulombs_law_force nil] (try (do (when (<= coulombs_law_radius 0.0) (throw (Exception. "radius must be positive"))) (set! coulombs_law_force (/ (* (* main_K coulombs_law_q1) coulombs_law_q2) (* coulombs_law_radius coulombs_law_radius))) (throw (ex-info "return" {:v coulombs_law_force}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (format2 (coulombs_law 15.5 20.0 15.0)))
      (println (format2 (coulombs_law 1.0 15.0 5.0)))
      (println (format2 (coulombs_law 20.0 (- 50.0) 15.0)))
      (println (format2 (coulombs_law (- 5.0) (- 8.0) 10.0)))
      (println (format2 (coulombs_law 50.0 100.0 50.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
