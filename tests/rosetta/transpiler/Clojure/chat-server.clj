(ns main (:refer-clojure :exclude [removeName main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare removeName main)

(declare main_clients removeName_out)

(defn removeName [removeName_names removeName_name]
  (try (do (def removeName_out []) (doseq [n removeName_names] (when (not= removeName_n removeName_name) (def removeName_out (conj removeName_out removeName_n)))) (throw (ex-info "return" {:v removeName_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn broadcast [broadcast_msg]
  (println broadcast_msg))

(defn add [add_name]
  (do (def add_clients (conj add_clients add_name)) (broadcast (str (str "+++ \"" add_name) "\" connected +++\n"))))

(defn send [send_name send_msg]
  (broadcast (str (str (str send_name "> ") send_msg) "\n")))

(defn remove [remove_name]
  (do (def remove_clients (removeName remove_clients remove_name)) (broadcast (str (str "--- \"" remove_name) "\" disconnected ---\n"))))

(defn main []
  (do (def main_clients []) (add "Alice") (add "Bob") (send "Alice" "Hello Bob!") (send "Bob" "Hi Alice!") (remove "Bob") (remove "Alice") (broadcast "Server stopping!\n")))

(defn -main []
  (main))

(-main)
