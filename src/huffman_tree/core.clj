(ns huffman-tree.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [malli.instrument :as mi]
            [malli.registry :as mr])
  (:gen-class))

;; ---------------------------------------------------------------------------------------------
;; ---------------------------------- Huffman Tree Functions: ----------------------------------
;; ---------------------------------------------------------------------------------------------

;; The node record will be the building block for our trees.
(defrecord Node [left right value count])

(mr/set-default-registry!
  (conj (m/default-schemas)
        {:Node [:fn #(instance? Node %)]}
        {:java-byte [:fn #(instance? java.lang.Byte %)]}
        {:io/InputStream [:fn #(instance? java.io.InputStream %)]}))

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

(defn lazy-input-stream
  "Reads an InputStream lazily into a sequence of bytes in efficient 4KB chunks, and closes the stream at the end."
  {:malli/schema
   [:function
    [:=> [:cat :io/InputStream] [:sequential :java-byte]]
    [:=> [:cat :io/InputStream bytes?] [:sequential :java-byte]]]}
  ([^java.io.InputStream stream]
   (lazy-input-stream stream (byte-array 4096))) ;; 4KB buffer.
  ([^java.io.InputStream stream ^bytes buffer]
   (lazy-seq
     (let [bytes-read (.read stream buffer)] ;; Read from the stream into a primitive array (buffer).
       (if (= bytes-read -1) (do (.close stream) nil) ;; EOF reached, close safely
         ;; 1. Loop to build a native Clojure transient vector from the primitive array.
         ;; This keeps allocation fast and avoids auto-boxing during the loop.
         (let [chunk-vec (loop [i 0 result (transient [])] ;; Start with a fast mutable vector.
                           (if (< i bytes-read)
                             (recur (unchecked-inc i) (conj! result (unchecked-byte (aget buffer i))))
                             (persistent! result)))] ;; At the end of the loop return an immutable vector.
           ;; 2. lazy-cat safely stitches the persistent vectors together.
           (lazy-cat chunk-vec (lazy-input-stream stream buffer))))))))

(defn lazy-file-seq
  "Opens an InputStream for the given file path and returns a lazy sequence of bytes.
  Reads data from disk in efficient 4KB chunks and yields elements lazily."
  {:malli/schema
   [:=> [:cat string?] [:sequential :java-byte]]}
  [file-path]
  (lazy-input-stream (io/input-stream file-path)))

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

(defn bit-seq->byte
  "Converts a seq of bits (0s and 1s) into a single byte.
  NOTE that the function expects a seq with 8 or less bits (0s and 1s).
  For example (bit-seq->byte [1 0 0 0 0 0 0 0]) returns -128"
  {:malli/schema
   [:=> [:cat [:sequential int?]] :java-byte]}
  [bit-seq]
  (let [pad-count (- 8 (count bit-seq))
        ;; Pad with trailing 0s if the bit-seq is shorter than 8 bits
        padded-group (concat bit-seq (repeat pad-count 0))]
    (unchecked-byte 
      (reduce (fn [byte-val bit] (+ (bit-shift-left byte-val 1) bit)) 0 padded-group))))

(defn byte->bit-seq
  "Converts a single byte into a vector of 8 bits (e.g., -128 would be [1 0 0 0 0 0 0 0])"
  {:malli/schema
   [:=> [:cat :java-byte] [:sequential int?]]}
  [the-byte]
  (mapv #(bit-and 1 (bit-shift-right the-byte %)) (range 7 -1 -1)))

(defn byte-seq->bit-seq
  "Takes a byte seq and converts it to a seq of bits (0s and 1s).
  Example: (byte-seq '(-128 -128)) returns [1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]"
  {:malli/schema
   [:=> [:cat [:sequential :java-byte] number?] [:sequential int?]]}
  [byte-arr total-bit-count]
  ;; Everything here is lazy. The mapcat is lazy and also the take is lazy.
  (let [all-bits (mapcat byte->bit-seq byte-arr)]
    ;; Take only the number of bits we need.
    ;; We must leave out any trailing zeros that were added as padding to fit the 8-bit boundary
    (take total-bit-count all-bits)))

(defn bits->megabytes
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
        (.write oos (int (bit-seq->byte group)))))))

(defn compress
  "Main compression function.
  Reads a file twice to build a Huffman tree and save a compressed version with O(1) RAM usage."
  {:malli/schema
   [:=> [:cat string? string?] nil?]}
  [input-file-path output-file-path]
  ;; Pass 1: Read the file and compute byte frequencies. The resulting map contains max 256 keys.
  (println "Calculating byte frequencies ...")
  (let [byte-frequencies (frequencies (lazy-file-seq input-file-path))
        ;; Now that we have the byte frequencies, we can create the Huffman tree:
        leafs (map #(->Node nil nil (first %) (second %)) byte-frequencies)
        leafs-sorted (sort-by :count leafs)
        huffman-tree (build-tree leafs-sorted)
        ;; Create a code table using the tree.
        code-table (make-code-table huffman-tree)
        ;; Calculate the compressed output's bit count by using the code-table and the frequency count.
        total-bit-count (reduce-kv
                          (fn [total byte freq] 
                            (+ total (* freq (count (get code-table byte))))) 
                          0 
                          byte-frequencies)
        output-data-mb (bits->megabytes total-bit-count)]
    (when (>= output-data-mb 2.0) 
      (println "The compressed file will be: " output-data-mb "MB in size."))
    ;; Pass 2: Open a fresh lazy stream and read the file all over again.
    ;; This time we are using the code-table to map the original bytes into compressed bits.
    ;; Every operation here is lazy.
    ;; Reading the file is lazy, mapping the bytes with mapcat is lazy and saving the result is lazy.
    (let [second-pass-bytes (lazy-file-seq input-file-path)
          all-bits-seq (mapcat #(get code-table %) second-pass-bytes)]
      (save-compressed-file output-file-path code-table total-bit-count all-bits-seq))))

;; ---------------------------------------------------------------------------------------------
;; ----------------------------------- Decompress Functions: -----------------------------------
;; ---------------------------------------------------------------------------------------------

(defn decompress-bit-seq
  "Converts a compressed seq of bits (0s and 1s) into an uncompressed lazy-seq of bytes by walking a Huffman tree."
  {:malli/schema
   [:function
    [:=> [:cat [:sequential int?] :Node]
         [:sequential :java-byte]]
    [:=> [:cat [:sequential int?] :Node [:map [:left map?] [:right map?]]]
         [:sequential :java-byte]]]}
  ([compressed-bit-seq huffman-tree]
   (decompress-bit-seq compressed-bit-seq huffman-tree huffman-tree))
  ([compressed-bit-seq huffman-tree current-node]
   (lazy-seq
    (when-let [bits (seq compressed-bit-seq)]
      (let [bit (first bits)
            next-node (if (= bit 0) (:left current-node) (:right current-node))]
        (if (and (nil? (:left next-node)) (nil? (:right next-node)))
          ;; Leaf reached: cons byte and lazily restart from the root with the 3-argument arity.
          (cons (:value next-node) (decompress-bit-seq (rest bits) huffman-tree huffman-tree))
          ;; Internal node reached: move deeper into the tree.
          (decompress-bit-seq (rest bits) huffman-tree next-node)))))))

(defn read-compressed-file
  "Reads the metadata and packed bytes safely from a compressed file."
  {:malli/schema
   [:=> [:cat string?]
    [:map [:code-table [:map-of :java-byte [:vector int?]]]
     [:total-bit-count number?]
     [:compressed-data [:sequential :java-byte]]]]}
  [file-path]
  (let [is (io/input-stream file-path)
        ois (java.io.ObjectInputStream. is)
        code-table (.readObject ois)
        total-bit-count (.readLong ois)]
    ;; Notice we are not closing the streams here, lazy-input-stream will do it.
    {:code-table code-table
     :total-bit-count total-bit-count
     :compressed-data (lazy-input-stream ois)}))

(defn decompress
  "Main decompression function.
  Reads a compressed file and returns a vector of decompressed bytes."
  {:malli/schema
   [:=> [:cat string?] [:sequential :java-byte]]}
  [file-path]
  (let [{:keys [code-table total-bit-count compressed-data]} (read-compressed-file file-path)
        bits (byte-seq->bit-seq compressed-data total-bit-count)]
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
      (and file-compress file-output)   (compress file-compress file-output)
      (and file-decompress file-output) (write-byte-seq-to-file (decompress file-decompress) file-output)
      :else (do (println "Usage: lein run [-c or -d] input-file [optional -o] output-file") 
                (println summary)))))
