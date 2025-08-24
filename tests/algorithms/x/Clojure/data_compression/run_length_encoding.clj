(ns main (:refer-clojure :exclude [run_length_encode run_length_decode]))

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

(declare run_length_encode run_length_decode)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic run_length_decode_ch nil)

(def ^:dynamic run_length_decode_i nil)

(def ^:dynamic run_length_decode_j nil)

(def ^:dynamic run_length_decode_num_str nil)

(def ^:dynamic run_length_decode_res nil)

(def ^:dynamic run_length_encode_encoded nil)

(def ^:dynamic run_length_encode_i nil)

(defn run_length_encode [run_length_encode_text]
  (binding [count_v nil run_length_encode_encoded nil run_length_encode_i nil] (try (do (when (= (count run_length_encode_text) 0) (throw (ex-info "return" {:v ""}))) (set! run_length_encode_encoded "") (set! count_v 1) (set! run_length_encode_i 0) (while (< run_length_encode_i (count run_length_encode_text)) (do (if (and (< (+ run_length_encode_i 1) (count run_length_encode_text)) (= (subs run_length_encode_text run_length_encode_i (+ run_length_encode_i 1)) (subs run_length_encode_text (+ run_length_encode_i 1) (+ (+ run_length_encode_i 1) 1)))) (set! count_v (+ count_v 1)) (do (set! run_length_encode_encoded (str (str run_length_encode_encoded (subs run_length_encode_text run_length_encode_i (+ run_length_encode_i 1))) (mochi_str count_v))) (set! count_v 1))) (set! run_length_encode_i (+ run_length_encode_i 1)))) (throw (ex-info "return" {:v run_length_encode_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_length_decode [run_length_decode_encoded]
  (binding [count_v nil run_length_decode_ch nil run_length_decode_i nil run_length_decode_j nil run_length_decode_num_str nil run_length_decode_res nil] (try (do (set! run_length_decode_res "") (set! run_length_decode_i 0) (while (< run_length_decode_i (count run_length_decode_encoded)) (do (set! run_length_decode_ch (subs run_length_decode_encoded run_length_decode_i (+ run_length_decode_i 1))) (set! run_length_decode_i (+ run_length_decode_i 1)) (set! run_length_decode_num_str "") (while (and (and (< run_length_decode_i (count run_length_decode_encoded)) (>= (compare (subs run_length_decode_encoded run_length_decode_i (+ run_length_decode_i 1)) "0") 0)) (<= (compare (subs run_length_decode_encoded run_length_decode_i (+ run_length_decode_i 1)) "9") 0)) (do (set! run_length_decode_num_str (str run_length_decode_num_str (subs run_length_decode_encoded run_length_decode_i (+ run_length_decode_i 1)))) (set! run_length_decode_i (+ run_length_decode_i 1)))) (set! count_v (toi run_length_decode_num_str)) (set! run_length_decode_j 0) (while (< run_length_decode_j count_v) (do (set! run_length_decode_res (str run_length_decode_res run_length_decode_ch)) (set! run_length_decode_j (+ run_length_decode_j 1)))))) (throw (ex-info "return" {:v run_length_decode_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_encoded1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_encoded2 nil)

(def ^:dynamic main_example3 nil)

(def ^:dynamic main_encoded3 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_example1) (constantly "AAAABBBCCDAA"))
      (alter-var-root (var main_encoded1) (constantly (run_length_encode main_example1)))
      (println main_encoded1)
      (println (run_length_decode main_encoded1))
      (alter-var-root (var main_example2) (constantly "A"))
      (alter-var-root (var main_encoded2) (constantly (run_length_encode main_example2)))
      (println main_encoded2)
      (println (run_length_decode main_encoded2))
      (alter-var-root (var main_example3) (constantly "AAADDDDDDFFFCCCAAVVVV"))
      (alter-var-root (var main_encoded3) (constantly (run_length_encode main_example3)))
      (println main_encoded3)
      (println (run_length_decode main_encoded3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
