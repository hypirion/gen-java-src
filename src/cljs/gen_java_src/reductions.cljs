(ns gen-java-src.reductions)

(defn variable-used?
  "Return true if the variable is used in codeblock, otherwise false."
  [var codeblock]
  (if (sequential? codeblock)
    (some #(variable-used? var %) codeblock)
    (= var codeblock))) ;; codeblock is not really a codeblock, heh.

(defn static-decl?
  [x]
  (and (sequential? x)
       (= (first x) :statics)))

(defn remove-unused-statics
  [[x name body]]
  (let [non-static-body (remove static-decl? body)
        unused-statics (-> ;; arrows man
                        (->> body
                             (filter static-decl?)
                             (apply concat)
                             (remove #(variable-used? % non-static-body))
                             set)
                        (disj :statics))]
    [x name
     (vec (for [block body]
            (if-not (static-decl? block)
              block
              (filterv #(not (contains? unused-statics %)) block))))]))

(defn reduce-class
  "Removes redundant code which, when left removed, have no impact on
  semantics."
  [class-ast]
  (-> class-ast
      remove-unused-statics))
