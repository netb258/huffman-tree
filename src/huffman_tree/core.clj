(ns huffman-tree.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(def test-file "EXAMPLE_FILE.txt")

(defn file-to-bytes
  "Takes a string that is a path to a file and
  returns the file as a vector of bytes."
  [file-path]
  (let [f (java.io.File. file-path)
        ary (byte-array (.length f))
        is (java.io.FileInputStream. f)]
    (.read is ary)
    (.close is)
    (into [] ary)))

;; The node record will be the building block for our trees.
(defrecord Node [left right value count])

(defn build-tree
  "This function will build a huffman tree.
  It needs a seq of all leafs (as Node records) in sorted order to do this.
  The resulting tree will be returned as a single Node record with all children nested inside it.
  Example input:
  '(Node{:left nil, :right nil, :value 97, :count 1}
    Node{:left nil, :right nil, :value 98, :count 2}
    Node{:left nil, :right nil, :value 99, :count 4})
  Example output:
   Node{
    :left Node{:left nil, :right nil, :value 99, :count 4},
    :right Node{
            :left Node{:left nil, :right nil, :value 97, :count 1},
            :right Node{:left nil, :right nil, :value 98, :count 2},
            :value nil,
            :count 3},
    :value nil,
    :count 7}"
  [sorted-leafs]
  (cond
    (empty? sorted-leafs) nil ;; The input cannot be empty.
    (empty? (rest sorted-leafs)) (first sorted-leafs) ;; If there is only one node remaining, then we are done.
    :else (let [node1 (first sorted-leafs)
                node2 (second sorted-leafs)]
            (recur (concat
                     ;; Remove the two nodes above.
                     (rest (rest sorted-leafs))
                     ;; Add one new node at the end.
                     (list (->Node node1 node2 nil (+ (:count node1) (:count node2)))))))))

(defn get-bit-patterns
  "Walks a huffman tree and returns a seq of maps with the bytes as keys and their bit patterns as a vector of 0 and 1.
  Expects a nested node data structure that represents a huffman tree.
  The tree is walked as per the specification here: https://en.wikipedia.org/wiki/Huffman_coding.
  Example input:
   Node{
    :left Node{:left nil, :right nil, :value 99, :count 4},
    :right Node{
            :left Node{:left nil, :right nil, :value 97, :count 1},
            :right Node{:left nil, :right nil, :value 98, :count 2},
            :value nil,
            :count 3},
    :value nil,
    :count 7}
  Example output: '({:byte 99, :bits [0]} {:byte 97, :bits [1 0]} {:byte 98, :bits [1 1]})"
  ([huffman-tree] (get-bit-patterns huffman-tree []))
  ([huffman-tree bit-path]
   (cond
     (and (nil? (:left huffman-tree))
          (nil? (:right huffman-tree))) (list {:byte (:value huffman-tree) :bits bit-path})
     :else (concat
             (get-bit-patterns (:left huffman-tree) (conj bit-path 0))
             (get-bit-patterns (:right huffman-tree) (conj bit-path 1))))))

(defn -main []
  (let [file-bytes (file-to-bytes test-file) ;; This will give us a vector of bytes.
        byte-counts (seq (frequencies file-bytes)) ;; This will give back a seq of [byte count], like this: ([97 1] [98 2] [99 4])
        leafs (map #(->Node nil nil (first %) (second %)) byte-counts)
        leafs-sorted (sort-by :count leafs)
        huffman-tree (build-tree leafs-sorted)
        compressed-bytes (get-bit-patterns huffman-tree)]
    (println "Original bytes: "
             (map #(str "{:byte " (first %) " :bits " (Integer/toBinaryString (first %)) "}") byte-counts))
    (println "Compressed bytes: " compressed-bytes)))
