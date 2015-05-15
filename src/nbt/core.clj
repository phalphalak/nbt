(ns nbt.core
  (:import [java.io
            FileInputStream
            DataInputStream
            ByteArrayInputStream]
           [java.util.zip GZIPInputStream InflaterInputStream]))

;; http://www.minecraftwiki.net/wiki/NBT_format
;; http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
;; https://github.com/Zeerix/NBT/blob/master/src/main/java/me/zeerix/nbt/NBTStreamReader.java
;; https://github.com/twoolie/NBT/blob/master/nbt/nbt.py

(def ^:const tags {0 :end
                   1 :byte
                   2 :short
                   3 :int
                   4 :long
                   5 :float
                   6 :double
                   7 :byte-array
                   8 :string
                   9 :tag-list
                   10 :compound
                   11 :int-array})

(defn read-type [^DataInputStream stream]
  (let [type-byte (.readByte stream)
        type-id (tags type-byte)]
    (if type-id
      type-id
      (throw (IllegalArgumentException. (str "Invalid NBT tag '" type-byte "'"))))))

(defn read-tag
  ([^DataInputStream stream]
     (read-tag stream (read-type stream)))
  ([^DataInputStream stream tag-type]
     (merge {:type tag-type}
            (case tag-type
              :end nil
              :byte {:payload (.readByte stream)}
              :short {:payload (.readShort stream)}
              :int {:payload (.readInt stream)}
              :long {:payload (.readLong stream)}
              :float {:payload (.readFloat stream)}
              :double {:payload (.readDouble stream)}
              :byte-array {:payload (byte-array (take (.readInt stream)
                                                      (repeatedly #(.readByte stream))))}
              :string {:payload (.readUTF stream)}
              :tag-list (let [list-type (read-type stream)
                              length (.readInt stream)]
                          {:list-type list-type
                           :payload (dotimes [i length]
                                      (read-tag stream list-type))})
              :compound {:payload (loop [acc {}]
                                    (let [type (read-type stream)]
                                      (if (= type :end)
                                        acc
                                        (let [name (.readUTF stream)
                                              payload (read-tag stream type)]
                                          (if (acc name)
                                            (throw (IllegalArgumentException. "Name collision"))
                                            (recur (assoc acc name payload)))))))}
              :int-array {:payload (int-array (take (.readInt stream)
                                                    (repeatedly #(.readInt stream))))}))))

(defn decode-nbt [^DataInputStream stream]
  (if (= :compound (read-type stream))
    (merge {:name (.readUTF stream)} (read-tag stream :compound))
    (throw (IllegalArgumentException. "Root NBT tag must be of type compound"))))

(defn ^DataInputStream ->stream [^String file & {:keys [gzip?]}]
  (cond-> (FileInputStream. file)
          gzip? (GZIPInputStream.)
          true (DataInputStream.)))

(defn decode-location [^DataInputStream stream]
  (let [data (.readInt stream)
        offset (* 4096 (bit-shift-right data 8))
        size (* 4096 (bit-and data 0xff))]
    {:offset offset
     :size size}))

(defn decode-locations [stream]
  (doall (filter identity
                 (repeatedly 1024
                             #(decode-location stream)))))

(defn decode-timestamp [^DataInputStream stream]
  (.readInt stream))

(defn decode-timestamps [stream]
  (doall (filter identity
                 (repeatedly 1024
                             #(decode-timestamp stream)))))

(defn decode-header [stream]
  (->> (map (fn [loc ts [x z]]
              (assoc loc
                :timestamp ts
                :x x
                :z z))
            (decode-locations stream)
            (decode-timestamps stream)
            (for [z (range 32)
                  x (range 32)]
              [x z]))
      (remove #(and (zero? (:offset %))
                    (zero? (:size %))))))

(defn decode-chunk [^DataInputStream stream sector-size]
  (let [length (.readInt stream)
        compression (case (.readByte stream)
                      1 :gzip
                      2 :zlib)
        compressed-chunk (byte-array length)
        _ (.read stream compressed-chunk)
        bais (ByteArrayInputStream. compressed-chunk)
        decompressing-input-stream (case compression
                                     :gzip (GZIPInputStream. bais)
                                     :zlib (InflaterInputStream. bais))
        nbt (decode-nbt (DataInputStream. decompressing-input-stream))]
    (.skipBytes stream (- sector-size 5 length))
    {:length length
     :compression compression
     :x nbt}))

(defn load-header [file]
  (with-open [stream (->stream "saves/New World/region/r.0.0.mca")]
    (decode-header stream)))

(defn load-region [file]
  (with-open [stream (->stream file)]
    (let [header (decode-header stream)
          chunck-order (sort-by :offset header)]
      (doall
       (reduce (fn [acc chunk-desc]
                 (.skipBytes stream (- (:offset chunk-desc)
                                       (:offset acc)))
                 (-> acc
                     (update-in [:header] conj (assoc chunk-desc
                                                 :chunk (decode-chunk stream
                                                                      (:size chunk-desc))))
                     (assoc :offset (+ (:offset chunk-desc)
                                       (:size chunk-desc)))))
               {:header []
                :offset 8192}
               (sort-by :offset header))))))

(defn load-chunk [file x z]
  (with-open [stream (->stream file)]
    (let [header (decode-header stream)
          chunk-desc (first (filter #(and (= (:x %) x)
                                          (= (:z %) z)) header))]
      (when chunk-desc
        (.skipBytes stream (- (:offset chunk-desc)
                              8192))
        (decode-chunk stream (:size chunk-desc))))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (prn (load-header "saves/New World/region/r.0.0.mca"))
;  (load-region "saves/New World/region/r.0.0.mca")
  (with-open [stream (->stream "fixture/test.nbt" :gzip? true)]
    (prn (decode-nbt stream)))
  (with-open [stream (->stream "fixture/bigtest.nbt" :gzip? true)]
    (prn (decode-nbt stream)))
  (comment (with-open [stream (->stream "fixture/Genesis/region/r.0.0.mca")]
             (prn (decode-nbt stream)))))
