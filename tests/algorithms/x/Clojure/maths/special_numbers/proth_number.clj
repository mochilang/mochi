(ns main (:refer-clojure :exclude [pow2 proth main]))

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

(declare pow2 proth main)

(def ^:dynamic main_n nil)

(def ^:dynamic main_value nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_result nil)

(def ^:dynamic proth_block nil)

(def ^:dynamic proth_block_index nil)

(def ^:dynamic proth_i nil)

(def ^:dynamic proth_increment nil)

(def ^:dynamic proth_next_val nil)

(def ^:dynamic proth_pow nil)

(def ^:dynamic proth_proth_index nil)

(def ^:dynamic proth_proth_list nil)

(def ^:dynamic proth_temp nil)

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_result nil] (try (do (set! pow2_result 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_result (* pow2_result 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn proth [proth_number]
  (binding [proth_block nil proth_block_index nil proth_i nil proth_increment nil proth_next_val nil proth_pow nil proth_proth_index nil proth_proth_list nil proth_temp nil] (try (do (when (< proth_number 1) (throw (Exception. "Input value must be > 0"))) (when (= proth_number 1) (throw (ex-info "return" {:v 3}))) (when (= proth_number 2) (throw (ex-info "return" {:v 5}))) (set! proth_temp (long (quot proth_number 3))) (set! proth_pow 1) (set! proth_block_index 1) (while (<= proth_pow proth_temp) (do (set! proth_pow (* proth_pow 2)) (set! proth_block_index (+ proth_block_index 1)))) (set! proth_proth_list [3 5]) (set! proth_proth_index 2) (set! proth_increment 3) (set! proth_block 1) (while (< proth_block proth_block_index) (do (set! proth_i 0) (while (< proth_i proth_increment) (do (set! proth_next_val (+ (pow2 (+ proth_block 1)) (nth proth_proth_list (- proth_proth_index 1)))) (set! proth_proth_list (conj proth_proth_list proth_next_val)) (set! proth_proth_index (+ proth_proth_index 1)) (set! proth_i (+ proth_i 1)))) (set! proth_increment (* proth_increment 2)) (set! proth_block (+ proth_block 1)))) (throw (ex-info "return" {:v (nth proth_proth_list (- proth_number 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_n nil main_value nil] (do (set! main_n 1) (while (<= main_n 10) (do (set! main_value (proth main_n)) (println (str (str (str "The " (str main_n)) "th Proth number: ") (str main_value))) (set! main_n (+ main_n 1)))))))

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
