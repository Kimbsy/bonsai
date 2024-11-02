(ns bonsai.sprites.branch-test
  (:require [bonsai.sprites.branch :as b]
            [clojure.test :refer :all]))

;; We start with a simple nested tree structure. Each node can have
;; any additional information we like (i.e. they can be quip sprite
;; objects), it also doesn't _need_ to be a binary tree, can have any
;; number of children.

;;              A
;;             / \
;;            /   \
;;           /     \
;;          X       Q
;;                 / \
;;                /   \
;;               /     \
;;              T       M
(def example-tree
  {:value 'A
   :children [{:value 'X
               :children []}
              {:value 'Q
               :children [{:value 'T
                           :children []}
                          {:value 'M
                           :children []}]}]})

;; Starting at the root node and going "anti-clockwise" around the
;; outside of the tree, add increasing numbers to the Left and Right
;; sides of each node as you reach them.

;;            0-A-9
;;             / \
;;            /   \
;;           /     \
;;        1-X-2   3-Q-8
;;                 / \
;;                /   \
;;               /     \
;;            4-T-5   6-M-7
(def expected-numbered-tree
  {:value 'A
   :L 0
   :R 9
   :children [{:value 'X
               :L 1
               :R 2
               :children []}
              {:value 'Q
               :L 3
               :R 8
               :children [{:value 'T
                           :L 4
                           :R 5
                           :children []}
                          {:value 'M
                           :L 6
                           :R 7
                           :children []}]}]})

;; We collapse the tree down into a flat sequence, removing the
;; `:children` from each node as the structure is now encoded in the
;; `:L` and `:R` values. We'll refer to this data representation as a
;; sequence of `branches` instead of a tree.

(def expected-branches
  [{:value 'A :L 0 :R 9}
   {:value 'X :L 1 :R 2}
   {:value 'Q :L 3 :R 8}
   {:value 'T :L 4 :R 5}
   {:value 'M :L 6 :R 7}])

(deftest nested-set-model

  (testing "Using the simplest example tree: "
    (let [tree {:value 'A :children []}
          numbered-tree (b/add-numbering tree)
          branches (b/collapse numbered-tree)]

      (testing "Tree is numbered correctly"
        (is (= {:value 'A :L 0 :R 1 :children []}
               numbered-tree)))

      (testing "Numbered tree is collapsed correctly"
        (is (= [{:value 'A :L 0 :R 1}]
               branches)))))

  (testing "Using a non-trivial example tree: "
    (let [tree example-tree
          numbered-tree (b/add-numbering tree)
          branches (b/collapse numbered-tree)]

      (testing "Tree is numbered correctly"
        (is (= expected-numbered-tree
               (b/add-numbering example-tree))))

      (testing "Numbered tree is collapsed correctly"
        (is (= expected-branches
               branches)))))

  (testing "Checking model invariants: "
    (let [branches (-> example-tree
                       b/add-numbering
                       b/collapse)]

      (testing "All values of `L` or `R` are unique"
        (let [L+R (mapcat (juxt :L :R) branches)]
          (apply distinct? L+R)))

      (testing "Every value of `L` is strictly less than it's corresponding value of `R`"
        (let [LRs (map (juxt :L :R) branches)]
          (is (every? (fn [[L R]]
                        (< L R))
                      LRs))))

      (testing "The difference between `L` and `R` values should be an odd number"
        (let [LRs (map (juxt :L :R) branches)]
          (is (every? (fn [[L R]]
                        (odd? (- R L)))
                      LRs))))))

  (testing "Various utilities for branches sequences: "
    (let [branches (-> example-tree
                       b/add-numbering
                       b/collapse)]

      (testing "We can determine the number of descendants of a node"
        (is (= 4 (b/descendant-count {:value 'A :L 0 :R 9})))
        (is (= 2 (b/descendant-count {:value 'Q :L 3 :R 8}))))

      (testing "We can get the descendants of a node"
        (is (= [{:value 'Q :L 3 :R 8}
                {:value 'T :L 4 :R 5}
                {:value 'M :L 6 :R 7}]
               (b/all-descendants branches {:value 'Q :L 3 :R 8}))))

      (testing "We can determine if a node has no children"
        (is (b/childless? {:value 'T :L 4 :R 5}))
        (is (not (b/childless? {:value 'Q :L 3 :R 8}))))

      (testing "We can find the direct children of a node"
        (is (= [{:value 'T :L 4 :R 5}
                {:value 'M :L 6 :R 7}]
               (b/direct-children branches {:value 'Q :L 3 :R 8})))
        (is (= [{:value 'X :L 1 :R 2}
                {:value 'Q :L 3 :R 8}]
               (b/direct-children branches {:value 'A :L 0 :R 9}))))

      (testing "We can get the direct parent of a node"
        (is (= {:value 'Q :L 3 :R 8}
               (b/parent branches {:value 'T :L 4 :R 5})))
        (is (= {:value 'A :L 0 :R 9}
               (b/parent branches {:value 'Q :L 3 :R 8}))))

      (testing "We can group nodes by depth"
        (is (= [[{:value 'A :L 0 :R 1}]]
               (b/group-by-depth [{:value 'A :L 0 :R 1}])))
        (is (= [[{:value 'A :L 0 :R 9}]
                [{:value 'X :L 1 :R 2} {:value 'Q :L 3 :R 8}]
                [{:value 'T :L 4 :R 5} {:value 'M :L 6 :R 7}]]
               (b/group-by-depth branches)))
        ))))

(deftest cutting-branch
  (testing "Removing branches: "
    (let [branches (-> example-tree
                       b/add-numbering
                       b/collapse)]

      (testing "can remove leaf nodes"
        (is (= [{:value 'A :L 0 :R 7}
                {:value 'X :L 1 :R 2}
                {:value 'Q :L 3 :R 6}
                {:value 'T :L 4 :R 5}]
               (b/cut branches {:value 'M :L 6 :R 7}))))

      (testing "can remove nested branches"
        (is (= [{:value 'A :L 0 :R 3}
                {:value 'X :L 1 :R 2}]
               (b/cut branches {:value 'Q :L 3 :R 8}))))

      (testing "can remove entire tree")
      (is (= [] (b/cut branches {:value 'A :L 0 :R 9}))))))

(deftest grafting-branch
  (testing "Adding branches: "
    (let [branches (-> example-tree
                       b/add-numbering
                       b/collapse)]

      (testing "can add leaf nodes"
        (let [new-branches [{:value 'F :L 0 :R 1}]]
          (is (= [{:value 'A :L 0 :R 11}
                  {:value 'X :L 1 :R 2}
                  {:value 'Q :L 3 :R 10}
                  {:value 'T :L 4 :R 5}
                  {:value 'M :L 6 :R 9}
                  {:value 'F :L 7 :R 8}]
                 (b/graft branches
                        {:value 'M :L 6 :R 7}
                        new-branches)))))

      (testing "can add nested branches"
        (let [new-branches [{:value 'F :L 0 :R 3}
                            {:value 'K :L 1 :R 2}]]
          (is (= [{:value 'A :L 0 :R 13}
                  {:value 'X :L 1 :R 2}
                  {:value 'Q :L 3 :R 12}
                  {:value 'T :L 4 :R 5}
                  {:value 'M :L 6 :R 11}
                  {:value 'F :L 7 :R 10}
                  {:value 'K :L 8 :R 9}]
                 (b/graft branches
                        {:value 'M :L 6 :R 7}
                        new-branches)))))

      ;; @TODO: add test showing that this works even if we're moving form a binary tree to an N-ary tree?
      )))
