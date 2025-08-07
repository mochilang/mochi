(ns main (:refer-clojure :exclude [contains backtrack word_break]))

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

(declare contains backtrack word_break)

(def ^:dynamic backtrack_end nil)

(def ^:dynamic backtrack_substr nil)

(defn contains [contains_words contains_target]
  (try (do (doseq [w contains_words] (when (= w contains_target) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn backtrack [backtrack_s backtrack_word_dict backtrack_start]
  (binding [backtrack_end nil backtrack_substr nil] (try (do (when (= backtrack_start (count backtrack_s)) (throw (ex-info "return" {:v true}))) (set! backtrack_end (+ backtrack_start 1)) (while (<= backtrack_end (count backtrack_s)) (do (set! backtrack_substr (subs backtrack_s backtrack_start (min backtrack_end (count backtrack_s)))) (when (and (contains backtrack_word_dict backtrack_substr) (backtrack backtrack_s backtrack_word_dict backtrack_end)) (throw (ex-info "return" {:v true}))) (set! backtrack_end (+ backtrack_end 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn word_break [word_break_s word_break_word_dict]
  (try (throw (ex-info "return" {:v (backtrack word_break_s word_break_word_dict 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (word_break "leetcode" ["leet" "code"])))
      (println (str (word_break "applepenapple" ["apple" "pen"])))
      (println (str (word_break "catsandog" ["cats" "dog" "sand" "and" "cat"])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
