(ns main)

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic main_current nil)

(def ^:dynamic main_door nil)

(def ^:dynamic main_incrementer nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_door 1)

(def ^:dynamic main_incrementer 0)

(defn -main []
  (doseq [main_current (range 1 101)] (do (def ^:dynamic main_line (str (str "Door " (mochi_str main_current)) " ")) (if (= main_current main_door) (do (alter-var-root (var main_line) (constantly (str main_line "Open"))) (alter-var-root (var main_incrementer) (constantly (+ main_incrementer 1))) (alter-var-root (var main_door) (constantly (+ (+ main_door (* 2 main_incrementer)) 1)))) (alter-var-root (var main_line) (constantly (str main_line "Closed")))) (println main_line))))

(-main)
