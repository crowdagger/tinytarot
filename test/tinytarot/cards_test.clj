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

(deftest test-total-value
  (testing "Checks that total value of cards is correct"
    (is (= 10.0
           (total-value [{:rank 3 :colour :heart}
                         {:rank 21 :colour :trump}
                         {:rank 14 :colour :diamond}
                         {:rank 2 :colour :diamond}])))))

(deftest test-gen-cards
  (testing "Checks that there are 78 unique cards"
    (is (= 78 (count (gen-cards))))
    (is (= 78 (count (into #{} (gen-cards)))))))