(ns main (:refer-clojure :exclude [to_bits quantum_fourier_transform]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare to_bits quantum_fourier_transform)

(declare _read_file)

(def ^:dynamic quantum_fourier_transform_counts nil)

(def ^:dynamic quantum_fourier_transform_i nil)

(def ^:dynamic quantum_fourier_transform_p nil)

(def ^:dynamic quantum_fourier_transform_per_state nil)

(def ^:dynamic quantum_fourier_transform_shots nil)

(def ^:dynamic quantum_fourier_transform_states nil)

(def ^:dynamic to_bits_num nil)

(def ^:dynamic to_bits_res nil)

(def ^:dynamic to_bits_w nil)

(defn to_bits [to_bits_n to_bits_width]
  (binding [to_bits_num nil to_bits_res nil to_bits_w nil] (try (do (set! to_bits_res "") (set! to_bits_num to_bits_n) (set! to_bits_w to_bits_width) (while (> to_bits_w 0) (do (set! to_bits_res (str (mochi_str (mod to_bits_num 2)) to_bits_res)) (set! to_bits_num (/ to_bits_num 2)) (set! to_bits_w (- to_bits_w 1)))) (throw (ex-info "return" {:v to_bits_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn quantum_fourier_transform [quantum_fourier_transform_number_of_qubits]
  (binding [quantum_fourier_transform_counts nil quantum_fourier_transform_i nil quantum_fourier_transform_p nil quantum_fourier_transform_per_state nil quantum_fourier_transform_shots nil quantum_fourier_transform_states nil] (try (do (when (<= quantum_fourier_transform_number_of_qubits 0) (throw (Exception. "number of qubits must be > 0."))) (when (> quantum_fourier_transform_number_of_qubits 10) (throw (Exception. "number of qubits too large to simulate(>10)."))) (set! quantum_fourier_transform_shots 10000) (set! quantum_fourier_transform_states 1) (set! quantum_fourier_transform_p 0) (while (< quantum_fourier_transform_p quantum_fourier_transform_number_of_qubits) (do (set! quantum_fourier_transform_states (* quantum_fourier_transform_states 2)) (set! quantum_fourier_transform_p (+ quantum_fourier_transform_p 1)))) (set! quantum_fourier_transform_per_state (/ quantum_fourier_transform_shots quantum_fourier_transform_states)) (set! quantum_fourier_transform_counts {}) (set! quantum_fourier_transform_i 0) (while (< quantum_fourier_transform_i quantum_fourier_transform_states) (do (set! quantum_fourier_transform_counts (assoc quantum_fourier_transform_counts (to_bits quantum_fourier_transform_i quantum_fourier_transform_number_of_qubits) quantum_fourier_transform_per_state)) (set! quantum_fourier_transform_i (+ quantum_fourier_transform_i 1)))) (throw (ex-info "return" {:v quantum_fourier_transform_counts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Total count for quantum fourier transform state is: " (mochi_str (quantum_fourier_transform 3))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
