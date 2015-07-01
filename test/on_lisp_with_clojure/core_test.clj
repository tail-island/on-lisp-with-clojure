(ns on-lisp-with-clojure.core-test
  (:use     (on-lisp-with-clojure core))
  (:require (clojure              [test :refer :all]))
  (:import  (java.lang            Math)))

(deftest test-if-match
  (is (= [1 2]
         (if-match [X Y 3 X] [1 2 3 1] [X Y])))
  (is (= [1 3]
         (if-match [X [2 Y]] [1 [2 3]] [X Y])))
  (is (= :not-match
         (if-match [X Y 3 X] [1 2 3 4] :match :not-match)))
  (is (= :not-match
         (if-match [1 2 3] [1 2 1] :match :not-match)))
  (is (= :not-match
         (if-match [1 2 3] [1 2 3 4] :match :not-match)))
  (is (= 2
         (if-match [_ X _] [1 2 3] X)))
  (is (= [3 4]
         (if-match [_ X _] [[1 2] [3 4] [5 6]] X :not-matched)))
  (is (= [nil 1]
         (if-match [X Y] [nil 1] [X Y])))
  (is (= [1 [2 3]]
         (if-match [X & Xs] [1 2 3] [X Xs])))
  (is (= [1 2 3]
         (if-match [& Xs] [1 2 3] Xs :not-matched)))
  (is (= [1 []]
         (if-match [X & Xs] [1] [X Xs])))
  (is (= [1 []]
         (if-match [[X & Xs] Xs] [[1] []] [X Xs])))
  (is (= [1 2 [3 4]]
         (if-match [X & [Y & Ys]] [1 2 3 4] [X Y Ys])))
  (is (= [1 2 3 [4]]
         (if-match [X & [Y1 Y2 & Ys]] [1 2 3 4] [X Y1 Y2 Ys])))
  (is (= :matched
         (if-match [& []] [] :matched)))
  (is (= :not-matched
         (if-match [[] & []] [] :matched :not-matched)))
  (is (= :not-matched
         (if-match [nil & []] [] :matched :not-matched))))

(deftest test-with-rules-and-query
  (with-rules (-> *rules*
                  (<- (append [] Xs Xs))
                  (<- (append [X & As] Bs [X & Cs])
                      (append As Bs Cs)))
    (is (= [[[] [:a :b :c :d]]
            [[:a]  [:b :c :d]]
            [[:a :b]  [:c :d]]
            [[:a :b :c]  [:d]]
            [[:a :b :c :d] []]]
           (with-inference (append X Y [:a :b :c :d])
             [X Y]))))

  (with-rules (-> *rules*
                  (<- (painter X)
                      (hungry? X)
                      (smells-of X "turpentine"))
                  (<- (hungry? X)
                      (or (gaunt X)
                          (eats-ravenously X)))
                  (<- (gaunt "raoul"))
                  (<- (smells-of "raoul" "turpentine"))
                  (<- (painter "rubens")))
    (is (= ["raoul" "rubens"]
           (with-inference (painter X)
             X))))
  
  (with-rules (-> *rules*
                  (<- (eats X F) (glutton X))
                  (<- (glutton "hubert"))
                  (<- (eats "monster" "bad-children"))
                  (<- (eats "warhol" "candy")))
    (is (= ["hubert"]
           (with-inference (eats X "spinach")
             X)))
    (is (= [["hubert" :unbound] ["monster" "bad-children"] ["warhol" "candy"]]
           (with-inference (eats X Y)
             [X (cond-> Y
                  (:unbound? (meta Y)) ((fn [_] :unbound)))]))))

  (with-rules (-> *rules*
                  (<- (all-elements X []))
                  (<- (all-elements X [X & Ys])
                      (all-elements X Ys)))
    (is (= [[] [:x] [:x :x]]
           (take 3 (with-inference (all-elements :x Ys)
                     Ys)))))

  (with-rules (-> *rules*
                  (<- (factorial 0 1))
                  (<- (factorial X F)
                      (clj (> X 0))
                      (is X' (- X 1))
                      (factorial X' F')
                      (is F (* X F'))))
    (is (= [40320]
           (with-inference (factorial 8 F)
             F))))

  (with-rules (-> *rules*
                  (<- (min X Y X)
                      (clj (<= X Y))
                      (cut))
                  (<- (min X Y Y)))
    (is (= [1]
           (with-inference (min 1 2 M)
             M)))
    (is (= [1]
           (with-inference (min 2 1 M)
             M))))

  (let [rules-0 (-> *rules*
                    (<- (artist X)
                        (sculptor X)
                        (cut))
                    (<- (artist X)
                        (painter X)))
        rules-1 (-> rules-0
                    (<- (painter "klee"))
                    (<- (painter "soutine")))
        rules-2 (-> rules-1
                    (<- (sculptor "hepworth")))]
    (with-rules rules-1
      (is (= ["klee" "soutine"]
             (with-inference (artist X)
               X))))
    (with-rules rules-2
      (is (= ["hepworth"]
             (with-inference (artist X)
               X)))))

  (let [rules-0 (-> *rules*
                    (<- (artist X)
                        (sculptor X)
                        (cut))
                    (<- (artist X)
                        (painter X)))
        rules-1 (-> rules-0
                    (<- (painter "klee"))
                    (<- (painter "soutine")))
        rules-2 (-> rules-1
                    (<- (sculptor "hepworth")))]
    (with-rules rules-1
      (is (= ["klee" "soutine"]
             (with-inference (artist X)
               X))))
    (with-rules rules-2
      (is (= ["hepworth"]
             (with-inference (artist X)
               X)))))

  (with-rules (-> *rules*
                  (<- (not-equal X X) (cut) (fail))
                  (<- (not-equal X Y)))
    (is (= []
           (with-inference (not-equal 1 1)
             :true)))
    (is (= [:true]
           (with-inference (not-equal 1 2)
             :true))))

  ;; From Wikipedia.
  
  (with-rules (-> *rules*
                  (<- (年齢 "山田" 35))
                  (<- (年齢 "大島" 20))
                  (<- (年齢 "清川" 28)))
    (is (= 83
           (reduce + (with-inference (年齢 X Y)
                       Y)))))  ; findallはないけれど、内部DSLなので外部でやっちゃえば大丈夫。

  (with-rules (-> *rules*
                  (<- (相加平均 ?標本リスト ?相加平均)
                      (相加平均 ?標本リスト 0 0 ?相加平均))
                  (<- (相加平均 [] ?標本数 ?累計 ?相加平均)
                      (clj (> ?標本数 0))
                      (is ?相加平均 (/ ?累計 ?標本数)))
                  (<- (相加平均 [?値 & R] ?標本数累計 ?累計 ?相加平均)
                      (is ?標本数累計' (inc ?標本数累計))
                      (is ?累計' (+ ?累計 ?値))
                      (相加平均 R ?標本数累計' ?累計' ?相加平均)))
    (is (= 70
           (first (with-inference (相加平均 [61 74 55 85 68 72 64 80 82 59] X)
                    X)))))

  (with-rules (-> *rules*
                  (<- (標準偏差 Xs V)
                      (is N (count Xs))
                      (is M (/ (reduce + Xs) N))
                      (標準偏差 Xs N M 0.0 V))
                  (<- (標準偏差 [] N M S V)
                      (is V (Math/sqrt (/ S N)))
                      (cut))
                  (<- (標準偏差 [X & Xs] N M S V)
                      (is S' (+ S (Math/pow (- X M) 2)))
                      (標準偏差 Xs N M S' V)))
    (is (= 9.777525249264253
           (first (with-inference (標準偏差 [61 74 55 85 68 72 64 80 82 59] V)
                    V)))))

  (with-rules (-> *rules*
                  (<- (最大値 [X & Xs] V)
                      (最大値 Xs X V))
                  (<- (最大値 [] V V))
                  (<- (最大値 [X & Xs] V' V)
                      (clj (> X V'))
                      (cut)
                      (最大値 Xs X V))
                  (<- (最大値 [X & Xs] V' V)
                      (最大値 Xs V' V)))
    (is (= [8]
           (with-inference (最大値 [2 4 6 8 3] V)
             V))))

  (let [rules-0 (-> *rules*
                    (<- (全ての要素が同じ [] _))
                    (<- (全ての要素が同じ [X & Xs] X)
                        (全ての要素が同じ Xs X)))
        rules-1 (-> rules-0
                    (<- (append [] Xs Xs))
                    (<- (append [X & As] Bs [X & Cs])
                        (append As Bs Cs)))
        rules-2 (-> rules-1
                    (<- (全ての要素が同じ-但し空リストは除く L A)
                        (全ての要素が同じ L A)
                        (not (clj (= L [])))))]
    (with-rules rules-0
      (is (= []
             (with-inference (全ての要素が同じ [1 2 1] X)
               X)))
      (is (= [1]
             (with-inference (全ての要素が同じ [1 1 1] X)
               X)))
      (is (= [[1 1 1]]
             (with-inference (全ての要素が同じ [X Y Z] 1)
               [X Y Z])))
      (is (= [[1 1 1]]
             (with-inference (全ての要素が同じ [A 1 B] X)
               [A B X]))))
    (with-rules rules-1
      (is (= []
             (first (with-inference (and (append L1 L2 [1 1 3 2]) (全ての要素が同じ L1 A))
                      L1)))))
    (with-rules rules-2
      (is (= [1]
             (first (with-inference (and (append L1 L2 [1 1 3 2]) (全ての要素が同じ-但し空リストは除く L1 A))
                      L1))))))

  (with-rules (-> *rules*
                  (<- (最大公約数 N ?最大公約数 ?最大公約数)
                      (not (clj (= ?最大公約数 0)))
                      (clj (= 0 (mod N ?最大公約数))))
                  (<- (最大公約数 N M ?最大公約数)
                      (not (clj (= M 0)))
                      (is M_2 (mod N M))
                      (最大公約数 M M_2 ?最大公約数)))
    (is (= [7]
           (with-inference (最大公約数 49 28 X)
             X))))

  (with-rules (-> *rules*
                  (<- (エラトステネスの篩 [] [])
                      (cut))
                  (<- (エラトステネスの篩 [A & R1] [A & R2])
                      (エラトステネスの篩 A R1 L)
                      (エラトステネスの篩 L R2))
                  (<- (エラトステネスの篩 _ [] [])
                      (cut))
                  (<- (エラトステネスの篩 N [A & R1] R2)
                      (clj (= 0 (mod A N)))
                      (エラトステネスの篩 N R1 R2)
                      (cut))
                  (<- (エラトステネスの篩 N [A & R1] [A & R2])
                      (エラトステネスの篩 N R1 R2)))
    (let [xs (nnext (range 32))]
      (is (= [[2 3 5 7 11 13 17 19 23 29 31]]
             (with-inference (エラトステネスの篩 xs Xs)
               Xs)))))
  
  (with-rules (-> *rules*
                  (<- (append [] L1 L1))
                  (<- (append [X & L1] L2 [X & L3])
                      (append L1 L2 L3))
                  (<- (append [] L2 L3 L4)
                      (append L2 L3 L4))
                  (<- (append [X & L1] L2 L3 [X & L4])
                      (append L1 L2 L3 L4)))
    (is (= [[[] :a :b [:c :d]]
            [[:a] :b :c [:d]]
            [[:a :b] :c :d []]]
           (with-inference (append L1 [X Y] L3 [:a :b :c :d])
             [L1 X Y L3]))))
  
  (with-rules (-> *rules*
                  (<- (append [] L1 L1))
                  (<- (append [X & L1] L2 [X & L3])
                      (append L1 L2 L3))
                  (<- (リスト要素の隣 ?リスト ?要素 ?隣の要素)
                      (append _ [?隣の要素 ?要素 & _] ?リスト))
                  (<- (リスト要素の隣 ?リスト ?要素 ?隣の要素)
                      (append _ [?要素 ?隣の要素 & _] ?リスト))
                  (<- (リスト要素の両隣 ?リスト ?要素 ?隣の要素_1 ?隣の要素_2)
                      (append _ [?隣の要素_1 ?要素 ?隣の要素_2 & _] ?リスト))
                  (<- (リスト要素の両隣 ?リスト ?要素 ?隣の要素_1 ?隣の要素_2)
                      (append _ [?隣の要素_2 ?要素 ?隣の要素_1 & _] ?リスト)))
    (is (= [[:b :a] [:c :b] [:a :b] [:b :c]]
           (with-inference (リスト要素の隣 [:a :b :c] X Y)
             [X Y])))
    (is (= []
           (with-inference (リスト要素の隣 [:a :b :c :d :e :f :g] :b :f)
             :matched)))
    (is (= [:matched]
           (with-inference (リスト要素の隣 [:a :b :c :d :e :f :g] :e :d)
             :matched)))
    (is (= [[:d :f] [:f :d]]
           (with-inference (リスト要素の両隣 [:a :b :c :d :e :f :g] :e Y Z)
             [Y Z]))))

  (with-rules (-> *rules*
                  (<- (append [] L1 L1))
                  (<- (append [X & L1] L2 [X & L3])
                      (append L1 L2 L3))
                  (<- (リスト要素の反転 [] []))
                  (<- (リスト要素の反転 [A & R1] L)
                      (リスト要素の反転 R1 L2)
                      (append L2 [A] L)))
    (is (= [[:c :b :a]]
           (with-inference (リスト要素の反転 [:a :b :c] L)
             L))))

  (with-rules (-> *rules*
                  (<- (member X [X & _]))
                  (<- (member X [_ & Xs])
                      (member X Xs))
                  (<- (組合せ X 1 [A])
                      (member A X))
                  (<- (組合せ [A & Y] N [A & X])
                      (clj (> N 1))
                      (is N_1 (- N 1))
                      (組合せ Y N_1 X))
                  (<- (組合せ [_ & Y] N A)
                      (clj (> N 1))
                      (組合せ Y N A)))
    (is (= [[:a :b :c] [:a :b :d] [:a :c :d] [:b :c :d]]
           (with-inference (組合せ [:a :b :c :d] 3 X)
             X))))
  
  ;; TODO: Add more!
  )
