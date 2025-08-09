(ns main (:refer-clojure :exclude [f make_points simpson_rule]))

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

(declare f make_points simpson_rule)

(def ^:dynamic make_points_points nil)

(def ^:dynamic make_points_x nil)

(def ^:dynamic simpson_rule_a nil)

(def ^:dynamic simpson_rule_b nil)

(def ^:dynamic simpson_rule_cnt nil)

(def ^:dynamic simpson_rule_coeff nil)

(def ^:dynamic simpson_rule_h nil)

(def ^:dynamic simpson_rule_i nil)

(def ^:dynamic simpson_rule_pts nil)

(def ^:dynamic simpson_rule_y nil)

(defn f [f_x]
  (try (throw (ex-info "return" {:v (* (- f_x 0.0) (- f_x 0.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_points [make_points_a make_points_b make_points_h]
  (binding [make_points_points nil make_points_x nil] (try (do (set! make_points_points []) (set! make_points_x (+ make_points_a make_points_h)) (while (< make_points_x (- make_points_b make_points_h)) (do (set! make_points_points (conj make_points_points make_points_x)) (set! make_points_x (+ make_points_x make_points_h)))) (throw (ex-info "return" {:v make_points_points}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simpson_rule [simpson_rule_boundary simpson_rule_steps]
  (binding [simpson_rule_a nil simpson_rule_b nil simpson_rule_cnt nil simpson_rule_coeff nil simpson_rule_h nil simpson_rule_i nil simpson_rule_pts nil simpson_rule_y nil] (try (do (when (<= simpson_rule_steps 0) (throw (Exception. "Number of steps must be greater than zero"))) (set! simpson_rule_a (nth simpson_rule_boundary 0)) (set! simpson_rule_b (nth simpson_rule_boundary 1)) (set! simpson_rule_h (quot (- simpson_rule_b simpson_rule_a) (double simpson_rule_steps))) (set! simpson_rule_pts (make_points simpson_rule_a simpson_rule_b simpson_rule_h)) (set! simpson_rule_y (* (/ simpson_rule_h 3.0) (f simpson_rule_a))) (set! simpson_rule_cnt 2) (set! simpson_rule_i 0) (while (< simpson_rule_i (count simpson_rule_pts)) (do (set! simpson_rule_coeff (- 4.0 (* 2.0 (double (mod simpson_rule_cnt 2))))) (set! simpson_rule_y (+ simpson_rule_y (* (* (/ simpson_rule_h 3.0) simpson_rule_coeff) (f (nth simpson_rule_pts simpson_rule_i))))) (set! simpson_rule_cnt (+ simpson_rule_cnt 1)) (set! simpson_rule_i (+ simpson_rule_i 1)))) (set! simpson_rule_y (+ simpson_rule_y (* (/ simpson_rule_h 3.0) (f simpson_rule_b)))) (throw (ex-info "return" {:v simpson_rule_y}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_result (simpson_rule [0.0 1.0] 10))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
