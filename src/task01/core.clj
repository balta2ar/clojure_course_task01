(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:require clojure.pprint)
  (:gen-class))

(defn extract [type args]
  (when (and (= type :h3)
             (= ((first args) :class) "r")
             (= 1 1))
    (:href (second (second args)))))

(defn process-tag [type & args]
  (let [sure-children (filter vector? args)
        children-types (apply concat (map #(apply process-tag %) sure-children))]
    (cons (extract type args) children-types)))

(defn links [data]
  (vec (filter (comp not nil?) (apply process-tag data))))

(defn get-links []
" 1) Find all elements containing {:class \"r\"}.

Example:
[:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                         :href \"https://github.com/clojure/clojure\",
                         :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                     [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

   2) Extract href from the element :a.

The link from the example above is 'https://github.com/clojure/clojure'.

  3) Return vector of all 10 links.

Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
"
  (let [data (parse "clojure_google.html")]
    (links data)))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))