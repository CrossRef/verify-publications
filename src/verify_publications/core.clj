(ns verify-publications.core
    (:require [clj-http.client :as client])
    (:require [clojure.data.json :as json]
               [clojure.set :as set])
    (:require [clojure.java.io :as io])
    (:require [clojure.data.csv :as csv])
    (:require [net.cgrand.enlive-html :as html]))

; "Interested name" is a name used to identify a publisher. Actual records may not match exactly but will contain the interested name.
(def interested-publishers #{"Elsevier" "Springer" "Wiley" "Taylor"})

; Util

(defn md5
  "Generate a md5 checksum for the given string"
  [token]
  (let [hash-bytes
         (doto (java.security.MessageDigest/getInstance "MD5")
               (.reset)
               (.update (.getBytes token)))]
       (.toString
         (new java.math.BigInteger 1 (.digest hash-bytes)) ; Positive and the size of the number
         16)))

(def cache-dir "/tmp/cache/")

(defn get-cached [url]
  (let [file-path (str cache-dir (md5 url))
        content (when (.exists (clojure.java.io/as-file file-path)) (slurp file-path))]
  (if (empty? content)
    (let [response (:body (client/get url))]
      (spit file-path response)
      response)
    content)))

; API fetching.

(def api-page-size 1000)
(def journal-url "http://api.crossref.org/journals?")

(defn journal-page-from-api [page]
  (let [response (get-cached (str journal-url (client/generate-query-string {:rows api-page-size :offset (* page api-page-size)})))
        items (-> (json/read-str response :key-fn keyword) :message :items)
        
        ; Remove the dash from :ISSN and call it :issn
        with-issns (map (fn [item] (merge {:issn (map (fn [issn] (apply str (remove #(= \- %) issn))) (:ISSN item))} item)) items)]
    with-issns))

(defn journal-pages-from-api
  "Return all journals from API."
  []
  (let [response (client/get journal-url)
        resp (json/read-str (:body response) :key-fn keyword)
        num-pages (/ (-> resp :message :total-results) api-page-size)
        ; num-pages 1
        pages (map journal-page-from-api (range num-pages))]
    (apply concat pages)))

(defn interested?
    "Return interested name if found or nil."
    [publisher-name]
    (some identity (map #(when (.contains publisher-name %) %) interested-publishers)))

(defn info-for-journals
  "For a collection of journals, retrieve interesting information."
  [journals]
  (let [all-issns (apply concat (map :issn journals))
        all-issn-count (count all-issns)
        journals-with-issn-count (count (remove #(empty? (:issn %)) journals))]
     {:count (count journals)
      :all-issns all-issns
      :all-issns-count all-issn-count
      :journals-with-issn-count journals-with-issn-count}))

; SCOPUS list.

(defn scopus-journals
  "Seq of [journal name, pissn, eissn, publisher name]"
  []
  (let [elsevier-file (io/reader (io/file (io/resource "title_list.csv")))
        ; drop first line as it's the title etc.
        journals (rest (csv/read-csv elsevier-file))]
    journals))

(defn scopus-info-for-publisher [publisher-name journals]
  (let [publisher-journals (filter #(.contains (get % 3) publisher-name) journals)
        
        ; Elements 
        print-issns (remove empty? (map second publisher-journals))
        e-issns (remove empty? (map #(get 2 %) publisher-journals))
        all-issns (concat print-issns e-issns)
        duplicate-issns (filter #(> (second %) 1) (frequencies all-issns))]
    
    {:journals publisher-journals
     :issns all-issns
     :unique-issns (set all-issns)
     :duplicate-issns duplicate-issns}))

(defn scopus-info-for-publishers []
  (let [journals (scopus-journals)
        for-interested-publishers (apply merge (map (fn [publisher-name] {publisher-name (scopus-info-for-publisher publisher-name journals)}) interested-publishers))]
    for-interested-publishers))

(def elsevier-base-url "http://www.elsevier.com/journals/title/")
(def elsevier-journals-domain "http://www.journals.elsevier.com")
(def elsevier-page-names ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "other"])

(defn issn-from-publication-page [url]
  (println "Fetch " url)
  (let [tds (map #(-> % :content first) (html/select (html/html-resource (java.net.URL. url)) [:div.ifTD]))        
        issns (map #(.substring % 6) (filter #(.contains % "ISSN: ") tds))]
    issns))

(defn publication-urls-from-page [url]
  (map #(-> % :attrs :href) (html/select (html/html-resource (java.net.URL. url)) [:div.product-list :ul.listing :li :a])))

(defn elsevier-website-claimed-info []
  ; We're just counting publication URLs.
  (let [alphabet-urls (map #(str elsevier-base-url %) elsevier-page-names)
        publication-urls (apply concat (map publication-urls-from-page alphabet-urls))
        ; Don't collect ISSNs.
        ; issns (apply concat (map #(issn-from-publication-page (rebase-url %)) publication-urls))
        ]
    {:num-journals (count publication-urls)}))

(defn -main
  [& args]
  (let [; From API.
        api-journals (journal-pages-from-api)
        api-journals-per-issn (apply merge (map (fn [journal] (apply merge (map (fn [issn] {issn journal}) (:issn journal)))) api-journals))
        api-interested-journals (filter #(-> % :publisher interested?) api-journals)
        
        ; Interested name => journals
        api-interested-name-journal (apply merge-with concat (map (fn [journal] {(-> journal :publisher interested?) [journal]}) api-interested-journals))
        api-interested-name-info (apply merge (map (fn [[interested-name journals]] {interested-name (info-for-journals journals)}) api-interested-name-journal))
 
        api-publisher-names (map :publisher api-journals)
        api-interested-publisher-names (map :publisher api-interested-journals)
        
        api-publisher-name-counts (frequencies api-publisher-names)
        api-interested-name-counts (frequencies api-interested-publisher-names)
        
        ; From SCOPUS.
        scopus-info (scopus-info-for-publishers)
        
        ; Elsevier
        
        ; From Elsevier website
        elsevier-web-info (elsevier-website-claimed-info)]
    
  (println "Found" (count api-journals) "journals in API")
  (println "Results from API")
  (doseq [[interested-name info] api-interested-name-info]
    (println "Name:" interested-name)
    (println "Journals" (:count info))
    (println "All ISSNs count" (:all-issns-count info))
    ; (println "All ISSNs" (:all-issns info))
    (println "Journals with ISSN" (:journals-with-issn-count info))
    (println))
  (println)
  
  (println "Results from SCOPUS list")
  (doseq [[interested-name info] scopus-info]
    (println "Name:" interested-name) 
      (println "Journals" (count (:journals info)))
      (println "ISSNs count" (count (:issns info)))
      (println "Unique ISSNs count" (count (:unique-issns info)))
      (println "Duplicate ISSNs count" (count (:duplicate-issns info)))
      (println "Duplicate ISSNs" (:duplicate-issns info))
      
      (println "ISSNs that are in our API" (count (filter api-journals-per-issn (:issns info))))
      (println "ISSNs that are not in our API" (count (filter #(not (api-journals-per-issn %)) (:issns info))))
      
      (println "ISSNs that our API thinks " interested-name " should claim but does not" (count (set/difference (:issns (get api-interested-name-info name)) (:issns info))))
      (println "ISSNs that " interested-name " claims but our API does not" (count (set/difference (:issns info) (:issns (get api-interested-name-info name)))))
      
      (println))
      
      (println "Elsevier website")
      (prn "Number of journals" (:num-journals elsevier-web-info))))
