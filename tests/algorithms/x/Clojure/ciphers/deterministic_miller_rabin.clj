(ns main (:refer-clojure :exclude [mod_pow miller_rabin]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare mod_pow miller_rabin)

(def ^:dynamic miller_rabin_bounds nil)

(def ^:dynamic miller_rabin_d nil)

(def ^:dynamic miller_rabin_i nil)

(def ^:dynamic miller_rabin_j nil)

(def ^:dynamic miller_rabin_last nil)

(def ^:dynamic miller_rabin_limit nil)

(def ^:dynamic miller_rabin_plist_len nil)

(def ^:dynamic miller_rabin_pr nil)

(def ^:dynamic miller_rabin_prime nil)

(def ^:dynamic miller_rabin_primes nil)

(def ^:dynamic miller_rabin_r nil)

(def ^:dynamic miller_rabin_s nil)

(def ^:dynamic miller_rabin_x nil)

(def ^:dynamic mod_pow_b nil)

(def ^:dynamic mod_pow_e nil)

(def ^:dynamic mod_pow_result nil)

(defn mod_pow [mod_pow_base mod_pow_exp mod_pow_mod]
  (binding [mod_pow_b nil mod_pow_e nil mod_pow_result nil] (try (do (set! mod_pow_result 1) (set! mod_pow_b (mod mod_pow_base mod_pow_mod)) (set! mod_pow_e mod_pow_exp) (while (> mod_pow_e 0) (do (when (= (mod mod_pow_e 2) 1) (set! mod_pow_result (mod (* mod_pow_result mod_pow_b) mod_pow_mod))) (set! mod_pow_b (mod (* mod_pow_b mod_pow_b) mod_pow_mod)) (set! mod_pow_e (quot mod_pow_e 2)))) (throw (ex-info "return" {:v mod_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn miller_rabin [miller_rabin_n miller_rabin_allow_probable]
  (binding [miller_rabin_bounds nil miller_rabin_d nil miller_rabin_i nil miller_rabin_j nil miller_rabin_last nil miller_rabin_limit nil miller_rabin_plist_len nil miller_rabin_pr nil miller_rabin_prime nil miller_rabin_primes nil miller_rabin_r nil miller_rabin_s nil miller_rabin_x nil] (try (do (when (= miller_rabin_n 2) (throw (ex-info "return" {:v true}))) (when (or (< miller_rabin_n 2) (= (mod miller_rabin_n 2) 0)) (throw (ex-info "return" {:v false}))) (when (> miller_rabin_n 5) (do (set! miller_rabin_last (mod miller_rabin_n 10)) (when (not (or (or (or (= miller_rabin_last 1) (= miller_rabin_last 3)) (= miller_rabin_last 7)) (= miller_rabin_last 9))) (throw (ex-info "return" {:v false}))))) (set! miller_rabin_limit 3825123056546413051) (when (and (> miller_rabin_n miller_rabin_limit) (not miller_rabin_allow_probable)) (throw (Exception. "Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test."))) (set! miller_rabin_bounds [2047 1373653 25326001 3215031751 2152302898747 3474749660383 341550071728321 miller_rabin_limit]) (set! miller_rabin_primes [2 3 5 7 11 13 17 19]) (set! miller_rabin_i 0) (set! miller_rabin_plist_len (count miller_rabin_primes)) (while (< miller_rabin_i (count miller_rabin_bounds)) (if (< miller_rabin_n (nth miller_rabin_bounds miller_rabin_i)) (do (set! miller_rabin_plist_len (+ miller_rabin_i 1)) (set! miller_rabin_i (count miller_rabin_bounds))) (set! miller_rabin_i (+ miller_rabin_i 1)))) (set! miller_rabin_d (- miller_rabin_n 1)) (set! miller_rabin_s 0) (while (= (mod miller_rabin_d 2) 0) (do (set! miller_rabin_d (quot miller_rabin_d 2)) (set! miller_rabin_s (+ miller_rabin_s 1)))) (set! miller_rabin_j 0) (while (< miller_rabin_j miller_rabin_plist_len) (do (set! miller_rabin_prime (nth miller_rabin_primes miller_rabin_j)) (set! miller_rabin_x (mod_pow miller_rabin_prime miller_rabin_d miller_rabin_n)) (set! miller_rabin_pr false) (if (or (= miller_rabin_x 1) (= miller_rabin_x (- miller_rabin_n 1))) (set! miller_rabin_pr true) (do (set! miller_rabin_r 1) (while (and (< miller_rabin_r miller_rabin_s) (not miller_rabin_pr)) (do (set! miller_rabin_x (mod (* miller_rabin_x miller_rabin_x) miller_rabin_n)) (when (= miller_rabin_x (- miller_rabin_n 1)) (set! miller_rabin_pr true)) (set! miller_rabin_r (+ miller_rabin_r 1)))))) (when (not miller_rabin_pr) (throw (ex-info "return" {:v false}))) (set! miller_rabin_j (+ miller_rabin_j 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (miller_rabin 561 false)))
      (println (str (miller_rabin 563 false)))
      (println (str (miller_rabin 838201 false)))
      (println (str (miller_rabin 838207 false)))
      (println (str (miller_rabin 17316001 false)))
      (println (str (miller_rabin 17316017 false)))
      (println (str (miller_rabin 3078386641 false)))
      (println (str (miller_rabin 3078386653 false)))
      (println (str (miller_rabin 1713045574801 false)))
      (println (str (miller_rabin 1713045574819 false)))
      (println (str (miller_rabin 2779799728307 false)))
      (println (str (miller_rabin 2779799728327 false)))
      (println (str (miller_rabin 113850023909441 false)))
      (println (str (miller_rabin 113850023909527 false)))
      (println (str (miller_rabin 1275041018848804351 false)))
      (println (str (miller_rabin 1275041018848804391 false)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
