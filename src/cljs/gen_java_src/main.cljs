(ns gen-java-src.main
  (:require [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [gen-java-src.generators :as jgen]
            [gen-java-src.pprint :as pp]
            [gen-java-src.reductions :refer [reduce-class]]
            [domina.xpath :refer [xpath]]
            [domina :as d]
            [clojure.browser.event :as event])
  (:require-macros [simple-check.properties :as prop]))

(defn htmlized-class
  "Could probably be optimised."
  [n]
  (let [r (nth (gen/sample-seq jgen/class-gen) n)]
    (pp/htmlify (reduce-class r))))

#_#_#_
(def codegen-button (d/by-id "btn"))
(def code-div (d/by-id "code-div"))

(event/listen codegen-button "click"
              (fn [] (let [level (-> (d/by-id "level") d/value str js/parseFloat
                                    Math/floor)]
                      (d/destroy-children! code-div)
                      (d/append! code-div (htmlized-class level)))))

