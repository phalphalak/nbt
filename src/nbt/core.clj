(ns nbt.core
  (:import (java.io FileInputStream
                    DataInputStream)
           (java.util.zip GZIPInputStream)))

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
                   10 :compound})

(defn parse-type [^DataInputStream stream]
  (let [type-byte (.readByte stream)
        type-id (tags type-byte)]
    (if type-id
      type-id
      (throw (IllegalArgumentException. (str "Invalid NBT tag '" type-byte "'"))))))

(defn parse-tag
  ([^DataInputStream stream]
     (parse-tag stream (parse-type stream)))
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
              :byte-array (throw (IllegalArgumentException. "TODO Not implemented yet"))
              :string {:payload (.readUTF stream)}
              :tag-list (let [list-type (parse-type stream)
                              length (.readInt stream)]
                          {:list-type list-type
                           :payload (dotimes [i length]
                                      (parse-tag stream list-type))})
              :compound {:payload (loop [acc {}]
                                    (let [type (parse-type stream)]
                                      (if (= type :end)
                                        acc
                                        (let [name (.readUTF stream)
                                              payload (parse-tag stream type)]
                                          (if (acc name)
                                            (throw (IllegalArgumentException. "Name collision"))
                                            (recur (assoc acc name payload)))))))}))))

(defn parse-nbt [^DataInputStream stream]
  (if (= :compound (parse-type stream))
    (merge {:name (.readUTF stream)} (parse-tag stream :compound))
    (throw (IllegalArgumentException. "Root NBT tag must be of type compound"))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (with-open [stream (DataInputStream. (GZIPInputStream. (FileInputStream. "fixture/Genesis/level.dat")))]
    (prn (parse-nbt stream)))
  (with-open [stream (DataInputStream. (GZIPInputStream. (FileInputStream. "fixture/Genesis/players/phalphalak.dat")))]
    (prn (parse-nbt stream)))
  (with-open [stream (DataInputStream. (GZIPInputStream. (FileInputStream. "fixture/Genesis/data/villages.dat")))]
    (prn (parse-nbt stream)))
  (comment (with-open [stream (DataInputStream. (FileInputStream. "fixture/Genesis/region/r.0.0.mca"))]
             (prn (parse-nbt stream)))))
