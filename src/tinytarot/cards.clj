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

(ns tinytarot.cards)

;; Basic functions for manipulating cards
;; There is no structure for cards, we just use a map with {rank, colour}

(defn oudler? 
  "Returns true if the card is an oudler, false else"
  [card]
  (let [{rank :rank colour :colour} card]
   (if (and (= colour :trump)
            (or (= rank 0)
                (= rank 1)
                (= rank 21)))
     true
     false)))

(defn excuse?
  "Return true if the card is the excuse, false else"
  [card]
  (if (and (= :trump (card :colour))
           (= 0 (card :rank)))
    true
    false))

(defn petit?
  "Return true if the card is 'Petit', false else"
  [card]
  (if (and (= :trump (card :colour))
           (= 1 (card :rank)))
    true
    false))

(defn better-than?
  "Return true if c1 wins other c2, false else"
  [c1 c2]
  (cond 
   (excuse? c1) false
   (excuse? c2) true
   (= (c1 :colour) (c2 :colour)) (> (c1 :rank) (c2 :rank))
   (= (c1 :colour) :trump) true
   :else false))

(defn value-of
  "Return the number of points a card grants"
  [card]
  (if (= :trump (card :colour))
    (if (oudler? card)
      4.5
      0.5)
    (condp = (card :rank)
      14 4.5 ; king
      13 3.5 ; queen
      12 2.5 ; knight
      11 1.5 ; jack
      0.5)))


;; Basic functions for manipulating list of cards

(defn gen-cards
  "Generate a list with all possible cards"
  []
  (into
   (for [c [:diamond :heart :spade :club]
         r (range 1 15)]
     {:rank r :colour c})
   (for [r (range 22)]
     {:rank r :colour :trump})))

(defn nb-oudlers
  "Returns the number of oudlers in the coll"
  [hand]
  (count (filter oudler? hand)))

(defn required-score
  "Returns the required score to win"
  [stack]
  (condp = (nb-oudlers stack)
    0 56
    1 51
    2 41
    3 36
    (throw (Exception. "Impossible number of oudlers"))))

(defn total-value 
  "Returns the total values of cards in a list"
  [coll]
  (apply + (map value-of coll)))