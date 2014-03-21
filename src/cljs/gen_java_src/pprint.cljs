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
(def ^:private class* (hilight :kw "class"))
(def ^:private static* (hilight :kw "static"))
(def ^:private int* (hilight :type "int"))
(def ^:private return* (hilight :kw "return"))
(def ^:private if* (hilight :kw "if"))
(def ^:private else* (hilight :kw "else"))
(def ^:private for* (hilight :kw "for"))


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
    [(indenting indent) int* " " (s/join ", " vars) ";\n"]))

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
  [indent [_ & vars]]
  (if (seq vars)
    [(indenting indent) public* " " static* " " int* " "
     (s/join ", " vars) ";\n"]))

(defmethod block-as-html :class
  [indent [_ name body]]
  (-> [(indenting indent)]
      (conj public* " " class* " " (hilight :classname name) " {\n")
      (into (mapcat #(block-as-html (inc indent) %) body))
      (conj (indenting indent) "}\n")))

(defmethod block-as-html :assignment
  [indent [_ var swap-op ast-expr]]
  (-> [(indenting indent)]
      (conj var " " (str (int-op swap-op) "=") " ")
      (into (as-html ast-expr))
      (conj ";\n")))

(defmethod block-as-html :if
  [indent [_ expr true-block false-block]]
  (let [spacing (indenting indent)]
    (-> [spacing if* " "]
        (into (as-html expr))
        (conj " {\n")
        (into (mapcat #(block-as-html (inc indent) %) true-block))
        (conj spacing "}\n")
        (cond->
         (seq false-block)
         (-> (conj spacing else* " {\n")
             (into (mapcat #(block-as-html (inc indent) %) false-block))
             (conj spacing "}\n"))))))

(defmethod block-as-html :for
  [indent [_ var upto block]]
  (let [spacing (indenting indent)]
    (-> [spacing for* " (" int* " " var " = " (hilight :number "0") "; "
         var " < " upto "; " var "++){\n"]
        (into (mapcat #(block-as-html (inc indent) %) block))
        (conj spacing "}\n"))))

(defn htmlify
  "This is probably not idiomatic."
  [class-ast]
  (hiccups/html [:pre (seq (block-as-html 0 class-ast))]))
