(ns on-lisp-with-clojure.core-test
  (:use     (on-lisp-with-clojure core))
  (:require (clojure              [test   :refer :all])))

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

  ;; TODO: Add more!
  )
