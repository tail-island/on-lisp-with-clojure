(ns on-lisp-with-clojure.core)

;; Utilities.

(defmacro cond-let
  [x & clauses]
  (when (seq clauses)
    `(if-let [~x ~(first clauses)]
       ~(if (next clauses)
          (second clauses)
          (throw (IllegalArgumentException. "cond-let requires fan even number of forms")))
       (cond-let ~x
         ~@(next (next clauses))))))

(defn lazy-mapcat
  [f colls]
  (for [coll colls, x (f coll)]
    x))

;; Matching.

(def ^:private ^:const first-variable-chars
  (set (cons \? (map char (take 26 (iterate inc (int \A)))))))

(defn- variable?
  [x]
  (and (symbol? x) (contains? first-variable-chars (first (name x)))))

(defn- variable-symbols
  [& xs]
  (filter variable? (tree-seq coll? identity xs)))

(defn match
  [variables x y]
  (letfn [(match-many [variables xs ys]
            (letfn [(destructure []  ; ドット対対策。
                      (let [ds (map #(if (= (first %) '&) (second %)) [xs ys])]
                        (if (some identity ds)
                          (map #(or %1 %2) ds [xs ys]))))]
              (cond-let it
                (destructure) (match variables (first it) (second it))
                :else         (if (every? seq [xs ys])
                                (if-let [variables (match variables (first xs) (first ys))]
                                  (recur variables (rest xs) (rest ys)))  ; nilではなく[]を使いたいので、nextではなくrestしました。
                                (match variables (seq xs) (seq ys))))))   ; シーケンスに変換しないと、[]の場合に無限ループしてしまいます……。
          (binded-values []
            (let [cs (map (partial contains? variables) [x y])]
              (if (some identity cs)
                (map #(if %1 (get variables %2) %2) cs [x y]))))
          (bind-values []
            (->> (map #(and (variable? %1) (assoc variables %1 %2)) [x y] [y x])
                 (some identity)))]  ; someで片方向にしているので、循環はしないはず……。Prologは定義に順序があるので、片方向でも大丈夫なはず……。On Lispでも片方向っぽいし……。
    (cond-let it
      (or (= x y) (= x '_) (= y '_)) variables
      (binded-values)                (recur variables (first it) (second it))
      (bind-values)                  it
      (every? coll? [x y])           (match-many variables x y))))

(defmacro with-gensym-variables-let
  [variables & body]
  `(let [~@(mapcat (fn [variable] [variable '(with-meta (gensym "?") {:unbound? true})]) variables)]
     ~@body))

(defn- quote-special-symbol
  [x]
  (if (and (coll? x) (not= (first x) 'quote))
    (vec (map quote-special-symbol x))
    (if (#{'_ '&} x)
      `(quote ~x)
      x)))

(defn variable-value
  [variables variable-symbol]
  (letfn [(get-value [variable-symbol]
            (letfn [(get-values [variable-values [variable-symbol & more :as variable-symbols]]
                      (if (seq variable-symbols)
                        (if (not= variable-symbol '&)  ; ドット対対策。
                          (recur (conj variable-values (get-value variable-symbol)) more)
                          (vec (concat variable-values (get-value (first more)))))
                        variable-values))]
              (if (and (variable? variable-symbol) (contains? variables variable-symbol))
                (let [variable-value (get variables variable-symbol)]
                  (if-not (coll? variable-value)
                    (recur variable-value)
                    (get-values [] variable-value)))
                variable-symbol)))]
    (get-value variable-symbol)))

(defmacro with-variables-let
  [variables & body]
  `(let [~@(->> (variable-symbols body)
                (mapcat (fn [variable-symbol] [variable-symbol `(variable-value ~variables ~variable-symbol)])))]
     ~@body))

(defmacro if-match
  [x y then & [else]]
  (let [variables (gensym)]
    `(with-gensym-variables-let ~(variable-symbols x y then)
       (if-let [~variables (match {} ~(quote-special-symbol x) ~(quote-special-symbol y))]
         (with-variables-let ~variables
           ~then)
         ~else))))

;; Prolog.

(def ^:dynamic *rules*
  [])

(defmacro with-rules
  [rules & body]
  `(binding [*rules* ~rules]
     ~@body))

(defn- compile-query-fn
  [expr]
  (let [variables (gensym)]
    (letfn [(compile-and-fn [[clause & more :as clauses]]
              (if (seq clauses)
                `(some->> (seq (~(compile-query-fn clause) ~variables))
                   (lazy-mapcat (fn [~variables] ~(compile-and-fn more))))
                `[~variables]))
            (compile-or-fn [clauses]
              `(concat ~@(map (fn [clause] `(~(compile-query-fn clause) ~variables)) clauses)))
            (compile-not-fn [expr]
              `(if-not (seq (~(compile-query-fn expr) ~variables))
                 [~variables]))
            (compile-is-fn [expr1 expr2]
              `(if-let [~variables (match ~variables ~expr1 (with-variables-let ~variables
                                                              ~expr2))]
                 [~variables]))
            (compile-cut-fn []
              `[~variables :cut])
            (compile-fail-fn []
              `nil)
            (compile-clj-fn [expr]
              `(if (with-variables-let ~variables
                     ~expr)
                 [~variables]))]
      `(fn [~variables]
         (if (= ~variables :cut)
           [~variables]
           ~(cond
              (= (first expr) 'and)  (compile-and-fn (next   expr))
              (= (first expr) 'or)   (compile-or-fn  (next   expr))
              (= (first expr) 'not)  (compile-not-fn (second expr))
              (= (first expr) 'is)   (compile-is-fn  (second expr) (nth expr 2))
              (= (first expr) 'cut)  (compile-cut-fn)
              (= (first expr) 'fail) (compile-fail-fn)
              (= (first expr) 'clj)  (compile-clj-fn (second expr))
              :else                  `(lazy-mapcat #(% ~variables ~(apply vector `(quote ~(first expr)) (quote-special-symbol (next expr)))) *rules*)))))))

(defn- compile-rule-fn
  [consequent antecedents]
  `(fn [variables# query#]
     (with-gensym-variables-let ~(variable-symbols consequent antecedents)
       (some->> (match variables# query# ~(apply vector `(quote ~(first consequent)) (quote-special-symbol (next consequent))))
         (~(compile-query-fn antecedents))))))

(defmacro <-
  [rules & [concequent & antecedents]]
  `(conj ~rules ~(compile-rule-fn concequent (cons 'and antecedents))))

(defmacro with-inference
  [query & body]
  `(with-gensym-variables-let ~(variable-symbols query body)
     (->> (~(compile-query-fn query) {})
          (take-while #(not= % :cut))
          (map #(with-variables-let %
                  ~@body)))))

;; TODO: ドット対への対応が、あまりにも場当たり的（2箇所に分散しちゃってるもんね……。あと、規則やクエリの中で&を使えなくなっちゃっているし……）。他の方法を検討してみる！
;; TODO: cutへの考慮に不足がないか見直す。テストを書けてないのでわからないけれど、明らかに不足しているような……。でも、実用上は問題なさそうな……。
