    ;; Copyright (C) 2012 Élisabeth Henry <liz.henry@ouvaton.org>

    ;; This file is part of TnT.

    ;; TnT is free software; you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published 
    ;; by the Free Software Foundation; either version 2 of the License,
    ;; or (at your option) any later version.

    ;; TnT is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License 
    ;; along with this software; if not, write to the Free Software
    ;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    ;; 02111-1307  USA.

(ns tinytarot.cards-test
  (:require [clojure.test :refer :all]
            [tinytarot.cards :refer :all]))

(deftest test-oudler
  (testing "Checks that 21 is oudler"
    (is (true? (oudler? {:rank 21 :colour :trump}))))
  (testing "Checks that 'Petit' is oudler"
    (is (true? (oudler? {:rank 1 :colour :trump}))))
  (testing "Checks that 'Excuse' is oudler"
    (is (true? (oudler? {:rank 0 :colour :trump}))))
  (testing "Checks that a random other card is not oudler"
    (is (false? (oudler? {:rank 8 :colour :diamond})))))

(deftest test-excuse
  (testing "Checks that the excuse returns true"
    (is (true? (excuse? {:rank 0 :colour :trump}))))
  (testing "Checks that some other card returns false"
    (is (false? (excuse? {:rank 12 :colour :trump})))))

(deftest test-petit
  (testing "Checks that the petit returns true"
    (is (true? (petit? {:rank 1 :colour :trump}))))
  (testing "Checks that some other card returns false"
    (is (false? (petit? {:rank 1 :colour :heart})))))

(deftest test-better
  (let [h3 {:colour :heart :rank 3}
        h7 {:colour :heart :rank 7}
        excuse {:colour :trump :rank 0}
        t3 {:colour :trump :rank 3}
        t7 {:colour :trump :rank 7}]
    (testing "Testing cards in the same colour"
      (is (true? (better-than? h7 h3)))
      (is (false? (better-than? h3 h7)))
      (is (true? (better-than? t7 t3)))
      (is (false? (better-than? t3 t7))))
    (testing "Checks that excuse never wins"
      (is (false? (better-than? excuse h3)))
      (is (false? (better-than? excuse h7)))
      (is (false? (better-than? excuse t3)))
      (is (true? (better-than? h3 excuse))))
    (testing "Checks behaviour when someone cuts"
      (is (true? (better-than? t3 h7)))
      (is (false? (better-than? h3 t3))))))

(deftest test-value
  (testing "Checks that value of cards is correct"
    (is (= 0.5 (value-of {:rank 3 :colour :heart})))
    (is (= 1.5 (value-of {:rank 11 :colour :diamond})))
    (is (= 4.5 (value-of {:rank 14 :colour :heart})))
    (is (= 0.5 (value-of {:rank 12 :colour :trump})))
    (is (= 4.5 (value-of {:rank 0 :colour :trump})))))

(deftest test-gen-cards
  (testing "Checks that there are 78 unique cards"
    (is (= 78 (count (gen-cards))))
    (is (= 78 (count (into #{} (gen-cards)))))))

(deftest test-nb-oudlers
  (testing "Checks that there are 3 oudlers in the game"
    (is (= 3 (nb-oudlers (gen-cards))))))

(deftest test-total-value
  (testing "Checks that total value of cards is correct"
    (is (= 10.0
           (total-value [{:rank 3 :colour :heart}
                         {:rank 21 :colour :trump}
                         {:rank 14 :colour :diamond}
                         {:rank 2 :colour :diamond}]))))
  (testing "Checks that total value of all cards is 91"
    (is (= 91.0 (total-value (gen-cards))))))

(deftest test-best
  (testing "Checks that we effectively get the cards that wins"
    (let [h3 {:rank 3 :colour :heart}
          h7 {:rank 7 :colour :heart}
          c5 {:rank 5 :colour :club}
          t2 {:rank 2 :colour :trump}]
      (is (= h7 (best-card [h3 h7 c5])))
      (is (= t2 (best-card [h3 t2 c5])))
      (is (= c5 (best-card [c5 h3 h7]))))))

(deftest test-playable
  (testing "Checks that playable? algorithm is ok"
    (let [h3 {:rank 3 :colour :heart}
          h7 {:rank 7 :colour :heart}
          c5 {:rank 5 :colour :club}
          t2 {:rank 2 :colour :trump}
          t3 {:rank 3 :colour :trump}
          all-trumps (for [r (range 1 15)]
                       {:rank r :colour :trump})
          all-diamonds (for [r (range 1 14)]
                         {:rank r :colour :diamond})]
      (is (false? (playable? {:rank 2 :colour :heart}
                             [{:rank 2 :colour :heart}
                              h7
                              t2]
                             [c5 t3])))
      
      (is (true? (playable? {:colour :diamond :rank 3}
                            all-diamonds
                            [t2 t3])))
      (is (false? (playable? t2
                             [t2 t3 {:colour :trump :rank 21}]
                             [{:colour :trump :rank 9}]))))))