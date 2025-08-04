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

(def ^:dynamic main_cells nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_rows nil)

(def ^:dynamic main_th nil)

(def ^:dynamic main_c (str (str (str (str (str "Character,Speech\n" "The multitude,The messiah! Show us the messiah!\n") "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") "The multitude,Who are you?\n") "Brians mother,I'm his mother; that's who!\n") "The multitude,Behold his mother! Behold his mother!"))

(def ^:dynamic main_rows [])

(def ^:dynamic main_headings true)

(defn -main []
  (doseq [line (split main_c "\n")] (def main_rows (conj main_rows (split line ","))))
  (println "<table>")
  (if main_headings (when (> (count main_rows) 0) (do (def ^:dynamic main_th "") (doseq [h (nth main_rows 0)] (def main_th (str (str (str main_th "<th>") h) "</th>"))) (println "   <thead>") (println (str (str "      <tr>" main_th) "</tr>")) (println "   </thead>") (println "   <tbody>") (def ^:dynamic main_i 1) (while (< main_i (count main_rows)) (do (def ^:dynamic main_cells "") (doseq [cell (nth main_rows main_i)] (def main_cells (str (str (str main_cells "<td>") cell) "</td>"))) (println (str (str "      <tr>" main_cells) "</tr>")) (def main_i (+ main_i 1)))) (println "   </tbody>"))) (doseq [row main_rows] (do (def ^:dynamic main_cells "") (doseq [cell row] (def main_cells (str (str (str main_cells "<td>") cell) "</td>"))) (println (str (str "    <tr>" main_cells) "</tr>")))))
  (println "</table>"))

(-main)
