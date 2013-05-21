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

(defn parse-type [stream]
  (let [type-byte (.readByte stream)
        type-id (tags type-byte)]
    (if type-id
      type-id
      (throw (IllegalArgumentException. (str "Invalid NBT tag '" type-byte "'"))))))

(defn parse-tag
  ([#^DataInputStream stream]
     (parse-tag stream (parse-type stream)))
  ([stream tag-id & {:keys [named?] :or {named? true}}]
     (let [name (when (and named? (not= tag-id :end)) (.readUTF stream))
           result (merge {:tag tag-id}
                         (when name {:name name}))]
       (merge result
              (condp = tag-id
                :end {}
                :byte {:value (.readByte stream)}
                :short {:value (.readShort stream)}
                :int {:value (.readInt stream)}
                :long {:value (.readLong stream)}
                :float {:value (.readFloat stream)}
                :double {:value (.readDouble stream)}
                :string {:value (.readUTF stream)}
                :tag-list (let [list-type (parse-type stream)
                                length (.readInt stream)]
                            {:list-type list-type
                             :value (dotimes [i length]
                                      (parse-tag stream list-type :named? false))})
                :compound {:value (loop [acc {}]
                                    (let [tag (parse-tag stream)]
                                      (if (= (tag :tag) :end)
                                        acc
                                        (if (acc (tag :name))
                                          (throw (IllegalArgumentException. "Name collision"))
                                          (recur (assoc acc (tag :name) tag))))))})))))

(defn parse-nbt [stream]
  (let [type-id (tags (.readByte stream))]
    (if (= :compound type-id)
      (parse-tag stream type-id)
      (throw (IllegalArgumentException. "Root NBT tag must be of type compound")))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (with-open [stream (DataInputStream. (GZIPInputStream. (FileInputStream. "fixture/Genesis/level.dat")))]
    (prn (parse-nbt stream)))
  )
