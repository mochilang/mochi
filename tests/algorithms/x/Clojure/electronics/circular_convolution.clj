(ns main (:refer-clojure :exclude [floor pow10 roundn pad circular_convolution]))

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

(declare floor pow10 roundn pad circular_convolution)

(def ^:dynamic circular_convolution_i nil)

(def ^:dynamic circular_convolution_idx nil)

(def ^:dynamic circular_convolution_j nil)

(def ^:dynamic circular_convolution_k nil)

(def ^:dynamic circular_convolution_n nil)

(def ^:dynamic circular_convolution_n1 nil)

(def ^:dynamic circular_convolution_n2 nil)

(def ^:dynamic circular_convolution_res nil)

(def ^:dynamic circular_convolution_sum nil)

(def ^:dynamic circular_convolution_x nil)

(def ^:dynamic circular_convolution_y nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic pad_s nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic roundn_m nil)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn roundn [roundn_x roundn_n]
  (binding [roundn_m nil] (try (do (set! roundn_m (pow10 roundn_n)) (throw (ex-info "return" {:v (/ (floor (+ (* roundn_x roundn_m) 0.5)) roundn_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad [pad_signal pad_target]
  (binding [pad_s nil] (try (do (set! pad_s pad_signal) (while (< (count pad_s) pad_target) (set! pad_s (conj pad_s 0.0))) (throw (ex-info "return" {:v pad_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn circular_convolution [circular_convolution_a circular_convolution_b]
  (binding [circular_convolution_i nil circular_convolution_idx nil circular_convolution_j nil circular_convolution_k nil circular_convolution_n nil circular_convolution_n1 nil circular_convolution_n2 nil circular_convolution_res nil circular_convolution_sum nil circular_convolution_x nil circular_convolution_y nil] (try (do (set! circular_convolution_n1 (count circular_convolution_a)) (set! circular_convolution_n2 (count circular_convolution_b)) (set! circular_convolution_n (if (> circular_convolution_n1 circular_convolution_n2) circular_convolution_n1 circular_convolution_n2)) (set! circular_convolution_x (pad circular_convolution_a circular_convolution_n)) (set! circular_convolution_y (pad circular_convolution_b circular_convolution_n)) (set! circular_convolution_res []) (set! circular_convolution_i 0) (while (< circular_convolution_i circular_convolution_n) (do (set! circular_convolution_sum 0.0) (set! circular_convolution_k 0) (while (< circular_convolution_k circular_convolution_n) (do (set! circular_convolution_j (mod (- circular_convolution_i circular_convolution_k) circular_convolution_n)) (set! circular_convolution_idx (if (< circular_convolution_j 0) (+ circular_convolution_j circular_convolution_n) circular_convolution_j)) (set! circular_convolution_sum (+ circular_convolution_sum (* (nth circular_convolution_x circular_convolution_k) (nth circular_convolution_y circular_convolution_idx)))) (set! circular_convolution_k (+ circular_convolution_k 1)))) (set! circular_convolution_res (conj circular_convolution_res (roundn circular_convolution_sum 2))) (set! circular_convolution_i (+ circular_convolution_i 1)))) (throw (ex-info "return" {:v circular_convolution_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_example3 nil)

(def ^:dynamic main_example4 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_example1) (constantly (circular_convolution [2.0 1.0 2.0 (- 1.0)] [1.0 2.0 3.0 4.0])))
      (println (str main_example1))
      (alter-var-root (var main_example2) (constantly (circular_convolution [0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6] [0.1 0.3 0.5 0.7 0.9 1.1 1.3 1.5])))
      (println (str main_example2))
      (alter-var-root (var main_example3) (constantly (circular_convolution [(- 1.0) 1.0 2.0 (- 2.0)] [0.5 1.0 (- 1.0) 2.0 0.75])))
      (println (str main_example3))
      (alter-var-root (var main_example4) (constantly (circular_convolution [1.0 (- 1.0) 2.0 3.0 (- 1.0)] [1.0 2.0 3.0])))
      (println (str main_example4))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
