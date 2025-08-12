(ns main (:refer-clojure :exclude [exp soboleva_modified_hyperbolic_tangent main]))

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

(declare exp soboleva_modified_hyperbolic_tangent main)

(def ^:dynamic exp_n nil)

(def ^:dynamic exp_sum nil)

(def ^:dynamic exp_term nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_vector nil)

(def ^:dynamic soboleva_modified_hyperbolic_tangent_denominator nil)

(def ^:dynamic soboleva_modified_hyperbolic_tangent_i nil)

(def ^:dynamic soboleva_modified_hyperbolic_tangent_numerator nil)

(def ^:dynamic soboleva_modified_hyperbolic_tangent_result nil)

(def ^:dynamic soboleva_modified_hyperbolic_tangent_x nil)

(defn exp [exp_x]
  (binding [exp_n nil exp_sum nil exp_term nil] (try (do (set! exp_term 1.0) (set! exp_sum 1.0) (set! exp_n 1) (while (< exp_n 20) (do (set! exp_term (/ (* exp_term exp_x) (to_float exp_n))) (set! exp_sum (+ exp_sum exp_term)) (set! exp_n (+ exp_n 1)))) (throw (ex-info "return" {:v exp_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn soboleva_modified_hyperbolic_tangent [soboleva_modified_hyperbolic_tangent_vector soboleva_modified_hyperbolic_tangent_a_value soboleva_modified_hyperbolic_tangent_b_value soboleva_modified_hyperbolic_tangent_c_value soboleva_modified_hyperbolic_tangent_d_value]
  (binding [soboleva_modified_hyperbolic_tangent_denominator nil soboleva_modified_hyperbolic_tangent_i nil soboleva_modified_hyperbolic_tangent_numerator nil soboleva_modified_hyperbolic_tangent_result nil soboleva_modified_hyperbolic_tangent_x nil] (try (do (set! soboleva_modified_hyperbolic_tangent_result []) (set! soboleva_modified_hyperbolic_tangent_i 0) (while (< soboleva_modified_hyperbolic_tangent_i (count soboleva_modified_hyperbolic_tangent_vector)) (do (set! soboleva_modified_hyperbolic_tangent_x (nth soboleva_modified_hyperbolic_tangent_vector soboleva_modified_hyperbolic_tangent_i)) (set! soboleva_modified_hyperbolic_tangent_numerator (- (exp (* soboleva_modified_hyperbolic_tangent_a_value soboleva_modified_hyperbolic_tangent_x)) (exp (* (- soboleva_modified_hyperbolic_tangent_b_value) soboleva_modified_hyperbolic_tangent_x)))) (set! soboleva_modified_hyperbolic_tangent_denominator (+ (exp (* soboleva_modified_hyperbolic_tangent_c_value soboleva_modified_hyperbolic_tangent_x)) (exp (* (- soboleva_modified_hyperbolic_tangent_d_value) soboleva_modified_hyperbolic_tangent_x)))) (set! soboleva_modified_hyperbolic_tangent_result (conj soboleva_modified_hyperbolic_tangent_result (/ soboleva_modified_hyperbolic_tangent_numerator soboleva_modified_hyperbolic_tangent_denominator))) (set! soboleva_modified_hyperbolic_tangent_i (+ soboleva_modified_hyperbolic_tangent_i 1)))) (throw (ex-info "return" {:v soboleva_modified_hyperbolic_tangent_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_res nil main_vector nil] (do (set! main_vector [5.4 (- 2.4) 6.3 (- 5.23) 3.27 0.56]) (set! main_res (soboleva_modified_hyperbolic_tangent main_vector 0.2 0.4 0.6 0.8)) (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) main_res)))))

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
