(ns huffman-tree.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [malli.instrument :as mi]
            [malli.registry :as mr])
  (:import [java.nio.file Files Paths])
  (:gen-class))

;; ---------------------------------------------------------------------------------------------
;; ---------------------------------- Huffman Tree Functions: ----------------------------------
;; ---------------------------------------------------------------------------------------------

;; The node record will be the building block for our trees.
(defrecord Node [left right value count])

(mr/set-default-registry!
  (conj (m/default-schemas)
        {:Node [:fn #(instance? Node %)]}
        {:java-byte [:fn #(instance? java.lang.Byte %)]}))

;; NOTE: This is probably not the most efficient way to build the huffman tree, but it shouldn't matter much.
;; A huffman tree's leafs are made up of every unique byte in a file.
;; Since a byte is 8 bits, there can only ever be 256 leafs. Meaning the tree will always be small.

(defn insert-sorted
  "Inserts a new Node (new-node) into a list of Node types (nodes-list-sorted).
   The inserted Node is places in sorted order (by :count) within the list. For example:
   (insert-sorted (list (->Node nil nil 97 1) (->Node nil nil 99 4)) (->Node nil nil 98 3))
   Will return:
   '(Node{:left nil, :right nil, :value 97, :count 1}
     Node{:left nil, :right nil, :value 98, :count 3}
     Node{:left nil, :right nil, :value 99, :count 4})"
  {:malli/schema
   [:=> [:cat [:sequential :Node] :Node]
        [:sequential :Node]]}
  [nodes-list-sorted new-node]
  (cond
    (empty? nodes-list-sorted) (list new-node)
    ;; If the count of the new-node is lower than the already sorted lowest node, then we cons it in front.
    (<= (:count new-node) (:count (first nodes-list-sorted))) (cons new-node nodes-list-sorted)
    ;; Otherwise cons the already sorted lowest node in front and recur with rest, we keep looking where to place new-node.
    :else (cons (first nodes-list-sorted) (insert-sorted (rest nodes-list-sorted) new-node))))

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
    :count 7}
  Basically, It's a binary tree where Nodes with the highest :count swim to the top."
  {:malli/schema
   [:=> [:cat [:sequential :Node]] :Node]}
  [sorted-leafs]
  (cond
    (empty? sorted-leafs) nil
    ;; This is the actual return value of build-tree. The master Node that contains all other nodes as children.
    (empty? (rest sorted-leafs)) (first sorted-leafs)
    :else (let [node1 (first sorted-leafs)
                node2 (second sorted-leafs)
                ;; Create a parent for the two above and they're all set.
                parent (->Node node1 node2 nil (+ (:count node1) (:count node2)))]
            ;; We remove the two nodes we processed from the list with (rest (rest).
            ;; However we still need the new parent we created, it could become the child of another Node.
            ;; We need to put it inside sorted-leafs and recur. Basically, we are removing two nodes and adding 1 in.
            ;; NOTE: That this function walks a sorted list and returns a sorted tree (by :count).
            ;; That means we can't just put the new parent at the top of the list and recur, We need to keep it sorted.
            (recur (insert-sorted (rest (rest sorted-leafs)) parent)))))

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
  Example output: '({:byte 97, :bits [0 0]} {:byte 98, :bits [0 1]} {:byte 99, :bits [1]})
  Since the leafs with the highest :count are at the top of the tree,
  they receive the shortest bit patter (:bits) when walking the tree."
  {:malli/schema 
   [:function
    [:=> [:cat :Node]
         [:sequential [:map [:byte :java-byte] [:bits [:vector int?]]]]]
    [:=> [:cat :Node [:vector int?]]
         [:sequential [:map [:byte :java-byte] [:bits [:vector int?]]]]]]}
  ([huffman-tree] (get-bit-patterns huffman-tree []))
  ([huffman-tree bit-path]
   (cond
     (and (nil? (:left huffman-tree))
          (nil? (:right huffman-tree))) (list {:byte (:value huffman-tree) :bits bit-path})
     :else (concat
             (get-bit-patterns (:left huffman-tree) (conj bit-path 0))
             (get-bit-patterns (:right huffman-tree) (conj bit-path 1))))))

(defn make-code-table
  "Basically summarizes a huffman tree in a table of {leaf bit-pattern}. EX: {97 [0 0], 98 [0 1], 99 [1]}
  The table contains all noes of the tree that have a :value and their bit patterns.
  The keys of the map are bytes and the compressed bit patterns are values.

  This way we can loop over every byte in a file and easily access it's compressed form.
  This table should also be all we need to return the compressed bytes back to their originals.
  Notice that if we invert the table: {[0 0] 97, [0 1] 98, [1] 99}
  Now we can access the original bytes by passing their compressed bits as keys.
  
  This table is always expected to be small.
  A huffman tree's leafs are made up of every unique byte in a file.
  Since a byte is 8 bits, there can only ever be 256 leafs."
  {:malli/schema
   [:=> [:cat :Node]
        [:map-of :java-byte [:vector int?]]]}
  [huffman-tree]
  (let [compressed-bytes (get-bit-patterns huffman-tree)] ;; Transform to: ({:byte 97, :bits [0 0]} {:byte 98, :bits [0 1]} ...)
    (reduce #(assoc %1 (:byte %2) (:bits %2)) {} compressed-bytes)))

(defn bit-to-key
  "Small helper function to convert bit values 0 and 1 into :left and :right keys."
  {:malli/schema
   [:=> [:cat int?] keyword?]}
  [bit]
  (cond
    (= bit 0) :left
    (= bit 1) :right
    :else (throw (IllegalArgumentException. (str "Invalid bit: " bit)))))

(defn code-table-to-huffman-tree
  "Reverse function. Converts a code table back to a Huffman tree."
  {:malli/schema
   [:=> [:cat [:map-of :java-byte [:vector int?]]] :Node]}
  [code-table]
  (reduce
    (fn [tree [byte-val bits-vector]]
      ;; Convert bit sequence to path of keys: [0 1] -> [:left :right]
      (let [path (map bit-to-key bits-vector)
            ;; The :value keyword needs to be included: [:left :right :value]
            ;; This way we can properly insert the byte.
            path-with-value (conj (vec path) :value)]
        (assoc-in tree path-with-value byte-val)))
    ;; Start with an empty base Node.
    (->Node nil nil nil nil)
    code-table))

;; ---------------------------------------------------------------------------------------------
;; -------------------------------------- Bits and Bytes: --------------------------------------
;; ---------------------------------------------------------------------------------------------

;; Notice how each function here has a mirror?
;; We need a function for compression a mirror function for decompression.

(defn file-to-byte-array
  "Uses the Java NIO library to efficiently read a file.
  Takes the path to the file and returns a primitive java byte array.
  Clojure has a wrapping mechanism to treat this array as a seq later on."
  {:malli/schema
   [:=> [:cat string?] bytes?]}
  [file-path]
  (Files/readAllBytes (Paths/get file-path (into-array String []))))

(defn write-byte-seq-to-file
  "Takes a lazy sequence of bytes and a string path, then 
  streams the bytes to the specified file in small chunks."
  {:malli/schema
   [:=> [:cat [:sequential :java-byte] string?] nil?]}
  [byte-seq file-path]
  (with-open [os (io/output-stream file-path)]
    ;; Partition the byte sequence into 4096-byte chunks (4KB)
    ;; partition-all is lazy, so this should  be memory efficient.
    (let [byte-chunks (partition-all 4096 byte-seq)]
      (doseq [chunk byte-chunks]
        ;; Convert the current byte chunk (which should be a seq of bytes) into a tiny java byte array.
        (let [ary (byte-array chunk)]
          (.write os ary))))))

(defn bit-seq-to-byte
  "Converts a seq of bits (0s and 1s) into a single byte.
  NOTE that the function expects a seq with 8 or less bits (0s and 1s).
  For example (bit-seq-to-byte [1 0 0 0 0 0 0 0]) returns -128"
  {:malli/schema
   [:=> [:cat [:sequential int?]] :java-byte]}
  [bit-seq]
  (let [pad-count (- 8 (count bit-seq))
        ;; Pad with trailing 0s if the bit-seq is shorter than 8 bits
        padded-group (concat bit-seq (repeat pad-count 0))]
    (unchecked-byte 
      (reduce (fn [byte-val bit] (+ (bit-shift-left byte-val 1) bit)) 0 padded-group))))

(defn byte-to-bit-seq
  "Converts a single byte into a vector of 8 bits (e.g., -128 would be [1 0 0 0 0 0 0 0])"
  {:malli/schema
   [:=> [:cat :java-byte] [:sequential int?]]}
  [the-byte]
  (mapv #(bit-and 1 (bit-shift-right the-byte %)) (range 7 -1 -1)))

(defn byte-array-to-bit-seq
  "Takes a byte array and converts it to a seq of bits (0s and 1s).
  Example: (byte-array [-128 -128]) returns [1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]"
  {:malli/schema
   [:=> [:cat bytes? number?] [:sequential int?]]}
  [byte-arr total-bit-count]
  (let [all-bits (mapcat byte-to-bit-seq byte-arr)]
    ;; Take only the number of bits we need.
    ;; We must leave out any trailing zeros that were added as padding to fit the 8-bit boundary
    (take total-bit-count all-bits)))

(defn bits-to-megabytes
  "Takes the number of bits and returns the number of megabytes.
  Works with floating point numbers.
  NOTE: We are dividing by 8388608, because:
  1MB = 1,024KB
  1,024KB = 1,048,576 bytes
  1,048,576 bytes = 8,388,608 bits"
  {:malli/schema
   [:=> [:cat number?] number?]}
  [bits]
  (/ (double bits) 8388608))

;; -------------------------------------------------------------------------------------
;; -------------------------------- Compress functions: --------------------------------
;; -------------------------------------------------------------------------------------

(defn save-compressed-file
  "Saves packed bytes and also metadata into a binary file.
  We can't just include the compressed bytes in the file, we need the metadata for decompression later.
  NOTE: We are serializing the entire code table as a Clojure data structure in the compressed file.
  This is very inefficient for tiny files.
  In order to have any potential memory gains, you must compress at least a 50kb file."
  {:malli/schema
   [:=> [:cat string? [:map-of :java-byte [:vector int?]] number? [:sequential int?]] nil?]}
  [file-path code-table total-bit-count all-bits-seq]
  (with-open [os (io/output-stream file-path)
              oos (java.io.ObjectOutputStream. os)]
    ;; Save the code table (needed for decoding)
    (.writeObject oos code-table)
    ;; Save the exact number of compressed bits
    (.writeLong oos total-bit-count)
    ;; Stream the bits to disk using partition-all/doseq. This way we process a lazy seq's members one at a time.
    (let [bit-groups (partition-all 8 all-bits-seq)]
      (doseq [group bit-groups]
        (.write oos (int (bit-seq-to-byte group)))))))

(defn compress
  "Main compressin function.
  Takes a primitive byte array and saves a compressed version of the bytes to a file."
  {:malli/schema
   [:=> [:cat bytes? string?] nil?]}
  [original-bytes output-file-path]
  (let [byte-frequencies (seq (frequencies original-bytes)) ;; This will give back a seq of [byte count], like this: ([97 1] [98 2] [99 4])
        ;; Build sorted leafs. Build tree. Make O(1) lookup table
        leafs (map #(->Node nil nil (first %) (second %)) byte-frequencies)
        leafs-sorted (sort-by :count leafs)
        huffman-tree (build-tree leafs-sorted)
        ;; Create a lookup table with original bytes and compressed bits: {97 [0 0], 98 [0 1], 99 [1]}
        code-table (make-code-table huffman-tree)
        ;; Map the original bytes to compressed bit vectors (vectors with 0s and 1s)
        ;; and flatten these vectors into a single seq of 0s and 1s. NOTE: Used to be flatten here, but mapcat is faster.
        all-bits-seq (mapcat #(get code-table %) original-bytes)
        ;; all-bits-seq (flatten (map #(get code-table %) original-bytes))
        ;; NOTE: This used to be just (count all-bits-seq), but that did not work on large files.
        ;; The issue is that the all-bits-seq can be very large. Now flatten and map return
        ;; a lazy seq so that is not an issue, but count attempts to realize the whole thing.
        ;; So now we need to calculate the bit count by using the code-table and the frequency count.
        total-bit-count (reduce-kv
                          (fn [total byte freq] 
                            (+ total (* freq (count (get code-table byte))))) 
                          0 
                          byte-frequencies)
        output-data-mb (bits-to-megabytes total-bit-count)]
    ;; If the output file is 2mb or bigger, tell the user we are working. Smaller files should be instantanious.
    (when (>= output-data-mb 2.0) (println "The compressed file will be: " output-data-mb "MB in size."))
    (save-compressed-file output-file-path code-table total-bit-count all-bits-seq)))

;; ---------------------------------------------------------------------------------------------
;; ----------------------------------- Decompress Functions: -----------------------------------
;; ---------------------------------------------------------------------------------------------

;; NOTE: Initially used this function, but it was too slow.
;; (defn decompress-bit-seq
;;   "Converts a compressed seq of bits (0s and 1s) into an uncompressed lazy-seq of bytes."
;;   {:malli/schema
;;    [:function
;;     [:=> [:cat [:sequential int?] [:map-of :java-byte [:vector int?]]]
;;          [:sequential :java-byte]]
;;     [:=> [:cat [:maybe [:sequential int?]] [:map-of [:vector int?] :java-byte] vector?]
;;          [:sequential :java-byte]]]}
;;   ;; Invert the table: converts {97 [0 0], 98 [0 1], 99 [1]} into {[0 0] 97, [0 1] 98, [1] 99}
;;   ([compressed-bit-seq code-table] (decompress-bit-seq (seq compressed-bit-seq) (clojure.set/map-invert code-table) []))
;;   ([compressed-bit-seq inverted-table current-pattern]
;;    (lazy-seq
;;      ;; We are using next for recursion, so if the seq is empty next will return nil.
;;      (when compressed-bit-seq
;;        (let [next-pattern (conj current-pattern (first compressed-bit-seq))
;;              matching-byte (get inverted-table next-pattern)]
;;          (if matching-byte
;;            ;; If a match is found, record the byte and clear the accumulator pattern
;;            (cons matching-byte (decompress-bit-seq (next compressed-bit-seq) inverted-table []))
;;            ;; If no match yet, keep accumulating bits
;;            (decompress-bit-seq (next compressed-bit-seq) inverted-table next-pattern)))))))

(defn decompress-bit-seq
  "Converts a compressed seq of bits (0s and 1s) into an uncompressed lazy-seq of bytes
   by traversing down the Huffman tree."
  {:malli/schema
   [:function
    [:=> [:cat [:sequential int?] :Node]
         [:sequential :java-byte]]
    [:=> [:cat [:sequential int?] :Node :Node]
         [:sequential :java-byte]]]}
  ([compressed-bit-seq huffman-tree]
   ;; Here we're passing compressed-bit-seq, huffman-tree, huffman-tree to start from the root of the tree.
   (decompress-bit-seq compressed-bit-seq huffman-tree huffman-tree))
  ([compressed-bit-seq huffman-tree current-node]
   (lazy-seq
     (loop [remaining-bits compressed-bit-seq
            node current-node]
       (when (seq remaining-bits)
         (let [bit (first remaining-bits)
               next-node (if (= bit 0) (:left node) (:right node))]
           (if (and (nil? (:left next-node)) (nil? (:right next-node)))
             ;; Leaf reached: cons byte and lazily restart from the root with the 3-argument arity.
             (cons (:value next-node) (decompress-bit-seq (rest remaining-bits) huffman-tree huffman-tree))
             ;; Internal node reached: move deeper into the tree using tail-recursion.
             (recur (rest remaining-bits) next-node))))))))

(defn read-compressed-file
  "Reads the metadata and packed bytes from a compressed file and returns them in a map."
  {:malli/schema
   [:=> [:cat string?]
        [:map [:code-table [:map-of :java-byte [:vector int?]]]
              [:total-bit-count number?]
              [:compressed-data bytes?]]]}
  [file-path]
  (with-open [is (io/input-stream file-path)
              ois (java.io.ObjectInputStream. is)
              baos (java.io.ByteArrayOutputStream.)]
    (let [code-table (.readObject ois)
          total-bit-count (.readLong ois)]
      ;; Stream all remaining bytes directly into the byte array output stream
      (.transferTo ois baos)
      {:code-table code-table
       :total-bit-count total-bit-count
       :compressed-data (.toByteArray baos)})))

(defn decompress
  "Main decompression function.
  Reads a compressed file and returns a vector of decompressed bytes."
  {:malli/schema
   [:=> [:cat string?] [:sequential :java-byte]]}
  [file-path]
  (let [{:keys [code-table total-bit-count compressed-data]} (read-compressed-file file-path)
        bits (byte-array-to-bit-seq compressed-data total-bit-count)]
    (println "Decompressing file ...")
    (decompress-bit-seq bits (code-table-to-huffman-tree code-table))))

;; ---------------------------------------------------------------------------------------------
;; ------------------------------------------- Main: -------------------------------------------
;; ---------------------------------------------------------------------------------------------

;; Use Malli to check function input/output.
;; (mi/collect!)
;; (mi/instrument! {:report (pretty/thrower)})

;; The command line options that our program accepts.
(def cli-options
  [["-c" "--compress FILE" "File to compress"]
   ["-d" "--decompress FILE" "File to decompress"]
   ["-o" "--output FILE" "Output file"]])

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        file-compress (:compress options)
        file-decompress (:decompress options)
        ;; Use the -o flag only if it is present.
        ;; The :arguments key of parse-opts contains any values that are not flags, and are not the parameters of a flag. 
        file-output (or (:output options) (first arguments))]
    (cond
      errors (println "Error:\n" (clojure.string/join "\n" errors))
      (and file-compress file-output)   (compress (file-to-byte-array file-compress) file-output)
      (and file-decompress file-output) (write-byte-seq-to-file (decompress file-decompress) file-output)
      :else (do (println "Usage: lein run [-c or -d] input-file [optional -o] output-file") 
                (println summary)))))
