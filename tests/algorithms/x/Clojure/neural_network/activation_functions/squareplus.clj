(ns main (:refer-clojure :exclude [sqrtApprox squareplus main]))

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

(declare sqrtApprox squareplus main)

(def ^:dynamic main_v1 nil)

(def ^:dynamic main_v2 nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic squareplus_i nil)

(def ^:dynamic squareplus_result nil)

(def ^:dynamic squareplus_val nil)

(def ^:dynamic squareplus_x nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn squareplus [squareplus_vector squareplus_beta]
  (binding [squareplus_i nil squareplus_result nil squareplus_val nil squareplus_x nil] (try (do (set! squareplus_result []) (set! squareplus_i 0) (while (< squareplus_i (count squareplus_vector)) (do (set! squareplus_x (nth squareplus_vector squareplus_i)) (set! squareplus_val (/ (+ squareplus_x (sqrtApprox (+ (* squareplus_x squareplus_x) squareplus_beta))) 2.0)) (set! squareplus_result (conj squareplus_result squareplus_val)) (set! squareplus_i (+ squareplus_i 1)))) (throw (ex-info "return" {:v squareplus_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_v1 nil main_v2 nil] (do (set! main_v1 [2.3 0.6 (- 2.0) (- 3.8)]) (set! main_v2 [(- 9.2) (- 0.3) 0.45 (- 4.56)]) (println (str (squareplus main_v1 2.0))) (println (str (squareplus main_v2 3.0))))))

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
