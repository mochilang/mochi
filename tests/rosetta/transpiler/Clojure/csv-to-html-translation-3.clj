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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn -main []
  (println "<table>")
  (println "   <thead>")
  (println "      <tr><th>Character</th><th>Speech</th></tr>")
  (println "   </thead>")
  (println "   <tbody>")
  (println "      <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>")
  (println "      <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>")
  (println "      <tr><td>The multitude</td><td>Who are you?</td></tr>")
  (println "      <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>")
  (println "      <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>")
  (println "   </tbody>")
  (println "</table>"))

(-main)
