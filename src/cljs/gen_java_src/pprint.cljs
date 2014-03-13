(ns gen-java-src.pprint
  (:require-macros [hiccups.core :as hiccups])
  (:require [clojure.string :as s]
            [hiccups.runtime :as hiccupsrt]))

(def block-space 2)

(defn indenting
  [n]
  (s/join (repeat (* block-space n) " ")))

(defn- hilight [type value]
  [:span {:class type} value])

(defmulti ^:private as-html
  #(try (first %) (catch js/Error e :default)))

(defmethod as-html :default
  [ast]
  [ast])

(defmethod as-html :compare
  [[_ op left right]]
  (-> ["("]
      (into (as-html left))
      (conj " " (hilight :cmp (name op)) " ")
      (into (as-html right))
      (conj ")")))

(def int-op
  {:+ "+" :- "-" :* "*" :div "/" :% "%"
   :xor "^" :and "&" :ior "|" :neg "~"})

(defmethod as-html :int-op
  [[_ op left right]]
  (-> ["("]
      (into (as-html left))
      (conj " " (hilight :op (int-op op)) " ")
      (into (as-html right))
      (conj ")")))

(defmulti block-as-html (fn [_ [x]] x))

(defmethod block-as-html :return
  [indent [_ ast]]
  (-> [(indenting indent) (hilight :reserved "return") " "]
      (into (as-html ast))
      (conj ";\n")))

(defmethod block-as-html :declare
  [indent [_ vars]]
  (-> [(indenting indent)]
      (conj (hilight :reserved "int") " ")
      (into (s/join ", " vars))
      (conj ";\n")))

(defmethod block-as-html :method
  [indent [_ m-name m-args body]]
  (-> [(indenting indent)]
      (conj (hilight :method m-name) "(")
      (into (butlast
             (interleave (repeat (hilight :reserved "int"))
                         (repeat " ") m-args (repeat ", "))))
      (conj ") {\n")
      (into (mapcat #(block-as-html (inc indent) %) body))
      (conj (first (indenting indent)) "}\n")))

(defmethod block-as-html :statics
  [indent [_ vars]]
  (-> [(indenting indent)]
      (conj (hilight :reserved "int") " ")
      (into vars)
      (conj ";\n")))

(defmethod block-as-html :class
  [indent [_ name body]]
  (-> [(indenting indent)]
      (conj (hilight :reserved "public class") " " name " {\n")
      (into (mapcat #(block-as-html (inc indent) %) body))
      (conj (indenting indent) "}\n")))

(defn htmlify
  "This is probably not idiomatic."
  [class-ast]
  (hiccups/html [:pre (seq (block-as-html 0 class-ast))]))
