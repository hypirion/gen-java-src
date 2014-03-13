(ns gen-java-src.main
  (:require [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [gen-java-src.generators :as jgen]
            [gen-java-src.pprint :as pp]
            [domina.xpath :refer [xpath]]
            [domina])
  (:require-macros [simple-check.properties :as prop]))

(def sort-idempotent-prop
  (prop/for-all [v (gen/vector gen/int)]
    (= (sort v) (sort (sort v)))))

(doseq [s (gen/sample jgen/class-gen)]
  (domina/append! (xpath "//body")
                  (pp/htmlify s)))
