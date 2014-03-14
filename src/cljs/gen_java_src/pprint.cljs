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

(def ^:private public* (hilight :kw "public"))
(def ^:private static* (hilight :kw "static"))
(def ^:private int* (hilight :type "int"))
(def ^:private return* (hilight :kw "return"))

(defmulti ^:private as-html
  #(try (first %) (catch js/Error e :default)))

(defmethod as-html :default
  [ast]
  (if (number? ast)
    [(hilight :number ast)]
    [ast]))

(defmethod as-html :compare
  [[_ op left right]]
  (-> ["("]
      (into (as-html left))
      (conj " " (hilight :cmp (name op)) " ")
      (into (as-html right))
      (conj ")")))

(def int-op
  {:+ "+" :- "-" :* "*" :div "/" :% "%"
   :xor "^" :and "&" :ior "|"})

(defmethod as-html :int-op
  [[_ op left right]]
  (-> ["("]
      (into (as-html left))
      (conj " " (hilight :op (int-op op)) " ")
      (into (as-html right))
      (conj ")")))

(defmethod as-html :invoke
  [[_ mname & args]]
  (-> [(hilight :invoke mname) "("]
      (into (butlast (mapcat #(conj (as-html %) ", ") args)))
      (conj ")")))

(defmulti block-as-html (fn [_ [x]] x))

(defmethod block-as-html :return
  [indent [_ ast]]
  (-> [(indenting indent) return* " "]
      (into (as-html ast))
      (conj ";\n")))

(defmethod block-as-html :declare
  [indent [_ vars]]
  (if (seq vars)
    (-> [(indenting indent)]
        (conj int* " ")
        (into (s/join ", " vars))
        (conj ";\n"))))

(defmethod block-as-html :method
  [indent [_ m-name m-args body]]
  (-> ["\n" (indenting indent)]
      (conj public* " " static* " "
            int* " " (hilight :method m-name) "(")
      (into (butlast
             (interleave (repeat int*) (repeat " ")
                         m-args (repeat ", "))))
      (conj ") {\n")
      (into (mapcat #(block-as-html (inc indent) %) body))
      (conj (indenting indent) "}\n")))

(defmethod block-as-html :statics
  [indent [_ vars]]
  (if (seq vars)
    (-> [(indenting indent)]
        (conj public* " "
              static* " "
              int*    " ")
        (into vars)
        (conj ";\n"))))

(defmethod block-as-html :class
  [indent [_ name body]]
  (-> [(indenting indent)]
      (conj public* " " (hilight :classname name) " {\n")
      (into (mapcat #(block-as-html (inc indent) %) body))
      (conj (indenting indent) "}\n")))

(defn htmlify
  "This is probably not idiomatic."
  [class-ast]
  (hiccups/html [:pre (seq (block-as-html 0 class-ast))]))
