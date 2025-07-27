(ns main (:refer-clojure :exclude [search_user main]))

(require 'clojure.set)

(defrecord Directory [username john])

(defrecord Client [Base Host Port GroupFilter])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare search_user main)

(defn search_user [directory username]
  (try (throw (ex-info "return" {:v (get directory username)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def client {"Base" "dc=example,dc=com" "Host" "ldap.example.com" "Port" 389 "GroupFilter" "(memberUid=%s)"}) (def directory {"username" ["admins" "users"] "john" ["users"]}) (def groups (search_user directory "username")) (if (> (count groups) 0) (do (def out "Groups: [") (def i 0) (while (< i (count groups)) (do (def out (str (str (str out "\"") (nth groups i)) "\"")) (when (< i (- (count groups) 1)) (def out (str out ", "))) (def i (+ i 1)))) (def out (str out "]")) (println out)) (println "User not found"))))

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
