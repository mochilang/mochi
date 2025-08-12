(ns main (:refer-clojure :exclude [inverse_of_matrix]))

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

(declare inverse_of_matrix)

(def ^:dynamic inverse_of_matrix_cof nil)

(def ^:dynamic inverse_of_matrix_det nil)

(def ^:dynamic inverse_of_matrix_i nil)

(def ^:dynamic inverse_of_matrix_inv nil)

(def ^:dynamic inverse_of_matrix_j nil)

(defn inverse_of_matrix [inverse_of_matrix_matrix]
  (binding [inverse_of_matrix_cof nil inverse_of_matrix_det nil inverse_of_matrix_i nil inverse_of_matrix_inv nil inverse_of_matrix_j nil] (try (do (if (and (and (= (count inverse_of_matrix_matrix) 2) (= (count (nth inverse_of_matrix_matrix 0)) 2)) (= (count (nth inverse_of_matrix_matrix 1)) 2)) (do (set! inverse_of_matrix_det (- (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 1) 1)) (* (nth (nth inverse_of_matrix_matrix 1) 0) (nth (nth inverse_of_matrix_matrix 0) 1)))) (when (= inverse_of_matrix_det 0.0) (do (println "This matrix has no inverse.") (throw (ex-info "return" {:v []})))) (throw (ex-info "return" {:v [[(/ (nth (nth inverse_of_matrix_matrix 1) 1) inverse_of_matrix_det) (/ (- (nth (nth inverse_of_matrix_matrix 0) 1)) inverse_of_matrix_det)] [(/ (- (nth (nth inverse_of_matrix_matrix 1) 0)) inverse_of_matrix_det) (/ (nth (nth inverse_of_matrix_matrix 0) 0) inverse_of_matrix_det)]]}))) (when (and (and (and (= (count inverse_of_matrix_matrix) 3) (= (count (nth inverse_of_matrix_matrix 0)) 3)) (= (count (nth inverse_of_matrix_matrix 1)) 3)) (= (count (nth inverse_of_matrix_matrix 2)) 3)) (do (set! inverse_of_matrix_det (- (+ (+ (* (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 1) 1)) (nth (nth inverse_of_matrix_matrix 2) 2)) (* (* (nth (nth inverse_of_matrix_matrix 0) 1) (nth (nth inverse_of_matrix_matrix 1) 2)) (nth (nth inverse_of_matrix_matrix 2) 0))) (* (* (nth (nth inverse_of_matrix_matrix 0) 2) (nth (nth inverse_of_matrix_matrix 1) 0)) (nth (nth inverse_of_matrix_matrix 2) 1))) (+ (+ (* (* (nth (nth inverse_of_matrix_matrix 0) 2) (nth (nth inverse_of_matrix_matrix 1) 1)) (nth (nth inverse_of_matrix_matrix 2) 0)) (* (* (nth (nth inverse_of_matrix_matrix 0) 1) (nth (nth inverse_of_matrix_matrix 1) 0)) (nth (nth inverse_of_matrix_matrix 2) 2))) (* (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 1) 2)) (nth (nth inverse_of_matrix_matrix 2) 1))))) (when (= inverse_of_matrix_det 0.0) (do (println "This matrix has no inverse.") (throw (ex-info "return" {:v []})))) (set! inverse_of_matrix_cof [[0.0 0.0 0.0] [0.0 0.0 0.0] [0.0 0.0 0.0]]) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [0 0] (- (* (nth (nth inverse_of_matrix_matrix 1) 1) (nth (nth inverse_of_matrix_matrix 2) 2)) (* (nth (nth inverse_of_matrix_matrix 1) 2) (nth (nth inverse_of_matrix_matrix 2) 1))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [0 1] (- (- (* (nth (nth inverse_of_matrix_matrix 1) 0) (nth (nth inverse_of_matrix_matrix 2) 2)) (* (nth (nth inverse_of_matrix_matrix 1) 2) (nth (nth inverse_of_matrix_matrix 2) 0)))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [0 2] (- (* (nth (nth inverse_of_matrix_matrix 1) 0) (nth (nth inverse_of_matrix_matrix 2) 1)) (* (nth (nth inverse_of_matrix_matrix 1) 1) (nth (nth inverse_of_matrix_matrix 2) 0))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [1 0] (- (- (* (nth (nth inverse_of_matrix_matrix 0) 1) (nth (nth inverse_of_matrix_matrix 2) 2)) (* (nth (nth inverse_of_matrix_matrix 0) 2) (nth (nth inverse_of_matrix_matrix 2) 1)))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [1 1] (- (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 2) 2)) (* (nth (nth inverse_of_matrix_matrix 0) 2) (nth (nth inverse_of_matrix_matrix 2) 0))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [1 2] (- (- (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 2) 1)) (* (nth (nth inverse_of_matrix_matrix 0) 1) (nth (nth inverse_of_matrix_matrix 2) 0)))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [2 0] (- (* (nth (nth inverse_of_matrix_matrix 0) 1) (nth (nth inverse_of_matrix_matrix 1) 2)) (* (nth (nth inverse_of_matrix_matrix 0) 2) (nth (nth inverse_of_matrix_matrix 1) 1))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [2 1] (- (- (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 1) 2)) (* (nth (nth inverse_of_matrix_matrix 0) 2) (nth (nth inverse_of_matrix_matrix 1) 0)))))) (set! inverse_of_matrix_cof (assoc-in inverse_of_matrix_cof [2 2] (- (* (nth (nth inverse_of_matrix_matrix 0) 0) (nth (nth inverse_of_matrix_matrix 1) 1)) (* (nth (nth inverse_of_matrix_matrix 0) 1) (nth (nth inverse_of_matrix_matrix 1) 0))))) (set! inverse_of_matrix_inv [[0.0 0.0 0.0] [0.0 0.0 0.0] [0.0 0.0 0.0]]) (set! inverse_of_matrix_i 0) (while (< inverse_of_matrix_i 3) (do (set! inverse_of_matrix_j 0) (while (< inverse_of_matrix_j 3) (do (set! inverse_of_matrix_inv (assoc-in inverse_of_matrix_inv [inverse_of_matrix_i inverse_of_matrix_j] (/ (nth (nth inverse_of_matrix_cof inverse_of_matrix_j) inverse_of_matrix_i) inverse_of_matrix_det))) (set! inverse_of_matrix_j (+ inverse_of_matrix_j 1)))) (set! inverse_of_matrix_i (+ inverse_of_matrix_i 1)))) (throw (ex-info "return" {:v inverse_of_matrix_inv}))))) (println "Please provide a matrix of size 2x2 or 3x3.") (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_m2 [[2.0 5.0] [2.0 0.0]])

(def ^:dynamic main_m3 [[2.0 5.0 7.0] [2.0 0.0 1.0] [1.0 2.0 3.0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (inverse_of_matrix main_m2))
      (println (inverse_of_matrix main_m3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
