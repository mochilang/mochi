(ns main (:refer-clojure :exclude [randN newField setCell state nextState newLife step lifeString main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randN newField setCell state nextState newLife step lifeString main)

(def ^:dynamic count_v nil)

(def ^:dynamic lifeString_out nil)

(def ^:dynamic lifeString_x nil)

(def ^:dynamic lifeString_y nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_l nil)

(def ^:dynamic newField_row nil)

(def ^:dynamic newField_rows nil)

(def ^:dynamic newField_x nil)

(def ^:dynamic newField_y nil)

(def ^:dynamic newLife_a nil)

(def ^:dynamic newLife_i nil)

(def ^:dynamic nextState_dx nil)

(def ^:dynamic nextState_dy nil)

(def ^:dynamic randN_seed nil)

(def ^:dynamic setCell_f nil)

(def ^:dynamic setCell_row nil)

(def ^:dynamic setCell_rows nil)

(def ^:dynamic state_x nil)

(def ^:dynamic state_y nil)

(def ^:dynamic step_l nil)

(def ^:dynamic step_tmp nil)

(def ^:dynamic step_x nil)

(def ^:dynamic step_y nil)

(def ^:dynamic main_seed 1)

(defn randN [randN_n]
  (binding [randN_seed nil] (try (do (set! randN_seed (mod (+ (* main_seed 1664525) 1013904223) 2147483647)) (throw (ex-info "return" {:v (mod randN_seed randN_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn newField [newField_w newField_h]
  (binding [newField_row nil newField_rows nil newField_x nil newField_y nil] (try (do (set! newField_rows []) (set! newField_y 0) (while (< newField_y newField_h) (do (set! newField_row []) (set! newField_x 0) (while (< newField_x newField_w) (do (set! newField_row (conj newField_row false)) (set! newField_x (+ newField_x 1)))) (set! newField_rows (conj newField_rows newField_row)) (set! newField_y (+ newField_y 1)))) (throw (ex-info "return" {:v {:s newField_rows :w newField_w :h newField_h}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn setCell [setCell_f_p setCell_x setCell_y setCell_b]
  (binding [setCell_f nil setCell_row nil setCell_rows nil] (do (set! setCell_f setCell_f_p) (set! setCell_rows (:s setCell_f)) (set! setCell_row (nth setCell_rows setCell_y)) (set! setCell_row (assoc setCell_row setCell_x setCell_b)) (set! setCell_rows (assoc setCell_rows setCell_y setCell_row)) (set! setCell_f (assoc setCell_f :s setCell_rows)))))

(defn state [state_f state_x_p state_y_p]
  (binding [state_x nil state_y nil] (try (do (set! state_x state_x_p) (set! state_y state_y_p) (while (< state_y 0) (set! state_y (+ state_y (:h state_f)))) (while (< state_x 0) (set! state_x (+ state_x (:w state_f)))) (throw (ex-info "return" {:v (nth (get (:s state_f) (mod state_y (:h state_f))) (mod state_x (:w state_f)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nextState [nextState_f nextState_x nextState_y]
  (binding [count_v nil nextState_dx nil nextState_dy nil] (try (do (set! count_v 0) (set! nextState_dy (- 1)) (while (<= nextState_dy 1) (do (set! nextState_dx (- 1)) (while (<= nextState_dx 1) (do (when (and (not (and (= nextState_dx 0) (= nextState_dy 0))) (state nextState_f (+ nextState_x nextState_dx) (+ nextState_y nextState_dy))) (set! count_v (+ count_v 1))) (set! nextState_dx (+ nextState_dx 1)))) (set! nextState_dy (+ nextState_dy 1)))) (throw (ex-info "return" {:v (or (= count_v 3) (and (= count_v 2) (state nextState_f nextState_x nextState_y)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn newLife [newLife_w newLife_h]
  (binding [newLife_a nil newLife_i nil] (try (do (set! newLife_a (newField newLife_w newLife_h)) (set! newLife_i 0) (while (< newLife_i (quot (* newLife_w newLife_h) 2)) (do (setCell newLife_a (randN newLife_w) (randN newLife_h) true) (set! newLife_i (+ newLife_i 1)))) (throw (ex-info "return" {:v {:a newLife_a :b (newField newLife_w newLife_h) :w newLife_w :h newLife_h}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn step [step_l_p]
  (binding [step_l nil step_tmp nil step_x nil step_y nil] (do (set! step_l step_l_p) (set! step_y 0) (while (< step_y (:h step_l)) (do (set! step_x 0) (while (< step_x (:w step_l)) (do (setCell (:b step_l) step_x step_y (nextState (:a step_l) step_x step_y)) (set! step_x (+ step_x 1)))) (set! step_y (+ step_y 1)))) (set! step_tmp (:a step_l)) (set! step_l (assoc step_l :a (:b step_l))) (set! step_l (assoc step_l :b step_tmp)))))

(defn lifeString [lifeString_l]
  (binding [lifeString_out nil lifeString_x nil lifeString_y nil] (try (do (set! lifeString_out "") (set! lifeString_y 0) (while (< lifeString_y (:h lifeString_l)) (do (set! lifeString_x 0) (while (< lifeString_x (:w lifeString_l)) (do (if (state (:a lifeString_l) lifeString_x lifeString_y) (set! lifeString_out (str lifeString_out "*")) (set! lifeString_out (str lifeString_out " "))) (set! lifeString_x (+ lifeString_x 1)))) (set! lifeString_out (str lifeString_out "\n")) (set! lifeString_y (+ lifeString_y 1)))) (throw (ex-info "return" {:v lifeString_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_i nil main_l nil] (do (set! main_l (newLife 80 15)) (set! main_i 0) (while (< main_i 300) (do (step main_l) (println "\f") (println (lifeString main_l)) (set! main_i (+ main_i 1)))))))

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
