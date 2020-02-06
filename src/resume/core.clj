(ns resume.core
  (:gen-class)
  (:refer-clojure :rename {map clj-map meta clj-meta time clj-time})
  (:require [clj-template.html5 :refer :all]
            [clojure.math.numeric-tower :as math]
            [resume.bio :as bio]))

;;;;;;;;;;;;;;;;;;;;
;; Get Data       ;;
;;;;;;;;;;;;;;;;;;;;

(defn get-data []
  (load-string (slurp "https://raw.githubusercontent.com/schmudde/schmud-de/master/src/schmud_de/models.clj")))

;;;;;;;;;;;;;;;;;;;;
;; Tools          ;;
;;;;;;;;;;;;;;;;;;;;

(defn desc-itemizer [matcher-object points]
  "I iterate through a java.util.regex.Matcher.find() object and turn each returned string into an HTML list item."
  (let [single-point (re-find matcher-object)]
    (if (some? single-point)
      (desc-itemizer matcher-object (str points (li {:class "qualification"} single-point)))
      points)))

(defn project-finder [search-term db keyword-search]
  (if (= ((first db) (keyword keyword-search)) search-term)
    (first db)
    (project-finder search-term (rest db) keyword-search)))

;;;;;;;;;;;;;;;;;;;;
;; Layouts        ;;
;;;;;;;;;;;;;;;;;;;;

(defn desc-layout [description]
  "I create a HTML list. I expect a string of data delimited by semicolons."
  (ul {:class "qualifications"} (desc-itemizer (re-matcher #"\w[^;]+" description) "")))

(defn column-layout [talent title description]
  "I take parameters and build a single entry"
  (div {:class "col-xs-4"}
       (h2 talent)
       (h3 title)
       (p description)))

(defn single-column-layout
  "I take parameters and build a single entry"
  [title subtitle description date & [class-synopsis bullet-points technology etc]]
  (div {:class "col-xs-12 job"}
       (h2 title (small {:class "pull-right"} date))
       (h3 description (small subtitle) (small technology))
       (p class-synopsis)
       (if (nil? bullet-points) nil (desc-layout bullet-points))
       etc))

(defn speaking-layout [talk]
  (li
   (p (strong (talk :title)))
   (p (talk :location))))

(defn minor-column-layout
  [title technology desc]
    (div
      (h4 title (small technology))
      (p desc)))

;;;;;;;;;;;;;;;;;;;;
;; Layouts        ;;
;;;;;;;;;;;;;;;;;;;;

(defn category-appender [category]
  (if category
    (str "TECHNOLOGIES: " category)
    category))

(defn minor-line-builder [institution]
  (let [{:keys [title technology desc]} institution]
    (minor-column-layout title (category-appender technology) desc)))

(defn line-builder [institution & [etc]]
  (let [{:keys [subtitle location title date synopsis desc technology]} institution]
    (single-column-layout subtitle location title date synopsis desc (category-appender technology) etc)))

(defn achievement-list-builder [achievements html-out template]
  (if (seq achievements)
    (achievement-list-builder (rest achievements)
                              (str html-out (template (first achievements)))
                              template)
    html-out))

(defn two-col-list
  "I take a list and build out an entire column"
  [list-column template]
  (div {:class "two-col-list"}
       (ul {:class "list-unstyled"}
        (achievement-list-builder list-column  "" template))))

(defn section-header [header-name & additional-class]
  (div {:class "col-xs-12"}
       (h4 {:class (str "text-center section-header " (first additional-class))} header-name)))

(def resume-footer
  (fn [] (footer {:class "footer"}
                (div {:class "container"}
                     (p "Made With Clojure &nbsp;|&nbsp; See the Source Code: "
                        (a {:href "http://bit.ly/2bWWDHc"} "http://bit.ly/2bWWDHc"))))))

;;;;;;;;;;;;;;;;;;;;
;; Layout: Header ;;
;;;;;;;;;;;;;;;;;;;;

(def resume-header
  (fn []
    (header {:class "row"}
            (div {:class "col-sm-6"}
                 (h1 (bio/biography :name))
                 (h2 (bio/biography :job-title)))
            (div {:class "col-sm-6"}
                 (div {:class "contact-info"}
                      (table {:class "table"}
                             (tr (td (span {:class "fa fa-envelope-o" :aria-hidden "true"})
                                     "&nbsp" (bio/biography :email))
                                 (td (span {:class "fa fa-globe" :aria-hidden "true"})
                                     "&nbsp;" (bio/biography :website)))
                             (tr (td (span {:class "fa fa-phone" :aria-hidden "true"})
                                     "&nbsp" (bio/biography :phone))
                                 (td (span {:class "fa fa-home" :aria-hidden "true"})
                                     "&nbsp;" (bio/biography :city)))))))))

;;;;;;;;;;;;;;;;;;;;
;; Layout: Awards ;;
;;;;;;;;;;;;;;;;;;;;

(defn award-layout [award]
  (li
   (if (= (award :highlight) "strong")
     (p {:class "highlight"} (strong (award :title)))
     (p (strong (award :title))))
   (p (award :org) "(" (award :type) ")")))

(def awards
  (fn [exhib] (let [award-list (exhib :awards)
               number-of-awards (count award-list)
               half (math/ceil (/ number-of-awards 2))
               html-awards ""]
           (div {:class "row"}
                (section-header "Selected Awards")
                (two-col-list (take half award-list) award-layout)
                (two-col-list (drop half award-list) award-layout)))))

;;;;;;;;;;;;;;;;;;;;
;; 3 Talents      ;;
;;;;;;;;;;;;;;;;;;;;

(def talent-column (fn [talent]
                     (column-layout ((bio/talent (keyword talent)) :title)
                                    ((bio/talent (keyword talent)) :domain)
                                    ((bio/talent (keyword talent)) :desc))))

(def talent
  (fn [] (div {:class "row"}
              (talent-column "educator")
              (talent-column "programmer")
              (talent-column "creative"))))

(def bio-summary
  (fn []
    (div {:class "row"}
         (section-header "Summary")
         (div {:class "col-xs-12"}
              (p (bio/biography :summary))))))

;;;;;;;;;;;;;;;;;;;;
;; Experience     ;;
;;;;;;;;;;;;;;;;;;;;

(def experience
  (fn [proj employ]
    (let [jack-machine (project-finder "Jack and the Machine" (proj :projects) "title")
          borderless   (project-finder "Borderless" (proj :projects) "title")
          rhythm       (project-finder "The Rhythm of Time" (proj :projects) "title")
          distant      (project-finder "Distant Apologies" (proj :projects) "title")
          btf          (project-finder "Beyond the Frame" (employ :employers) "subtitle")
          nextjournal  (project-finder "Nextjournal" (employ :employers) "subtitle")
          penguin      (project-finder "Penguin Random House" (employ :employers) "subtitle")
          netgalley    (project-finder "NetGalley" (employ :employers) "subtitle")
          ef-sharp     (project-finder "F#" (employ :employers) "subtitle")]

      (div {:class "row"}
           (section-header "Selected Experience" "experience-section-header")

           (reduce str (clojure.core/map #(line-builder %) [nextjournal]))

           (->> (clojure.core/map #(minor-line-builder %) [jack-machine rhythm distant])
                (reduce str)
                (div {:style "padding-left: 2.5em;"})
                (line-builder btf))

           (reduce str (clojure.core/map #(line-builder %) [penguin netgalley ef-sharp]))))))

;;;;;;;;;;;;;;;;;;;;
;; Academic       ;;
;;;;;;;;;;;;;;;;;;;;

;; Update database

(def academy
  (fn [exhib talk] (let [nu   (project-finder "Northwestern University" (exhib :education) "subtitle")
                         uni  (project-finder "University of Northern Iowa" (exhib :education) "subtitle")
                         stevens (project-finder "Stevens Institute of Technology" (talk :talks) "subtitle")
                         ia   (project-finder "Illinois Institute of Art" (talk :talks) "subtitle")
                         ccc  (project-finder "Columbia College Chicago" (talk :talks) "subtitle")
                         iadt (project-finder "International Academy of Design and Technology" (talk :talks) "subtitle")]
           (div {:class "row"}
                (section-header "Selected Academic Experience")
                (div {:class "row"}
                     (column-layout (stevens :subtitle) (stevens :title) (stevens :location))
                     (column-layout (ia :subtitle) (ia :title) (ia :location))
                     (column-layout (iadt :subtitle) (iadt :title) (iadt :location)))
                (div {:class "row"}
                     (column-layout (ccc :subtitle) (ccc :title) (ccc :location))
                     (column-layout (nu :subtitle) (nu :title) (nu :concentrations))
                     (column-layout (uni :subtitle) (uni :title) (uni :concentrations)))
))))

(def academy-horiz
  (fn [exhib] (let [nu (project-finder "Northwestern University" (exhib :education) "subtitle")
               uni (project-finder "University of Northern Iowa" (exhib :education) "subtitle")]
           (div {:class "row"}
                (section-header "Education")
                (reduce str (clojure.core/map #(line-builder %) [nu uni]))
                ))))

(def public-speaking
  (fn [talk]
    (let [curry-on (project-finder "Curry On, London, England" (talk :talks) "location")
          pycon    (project-finder "PyCon/PyData, Berlin, Germany" (talk :talks) "location")
          computation   (project-finder "Society for the History of Technology, Milan, Italy" (talk :talks) "location")
          internet      (project-finder "New York City" (talk :talks) "location")
          strangeloop   (project-finder "Strange Loop, St. Louis, MO" (talk :talks) "location")
          creative-code (project-finder "Creative Coding NYC" (talk :talks) "location")
          anthropology (project-finder "Enabling Digital Anthropology" (talk :talks) "title")
          intended    (project-finder "Intended Knowledge?" (talk :talks) "title")
          sigcis      (project-finder "Stored In Memory: The 10th Annual SIGCIS Conference, St. Louis, MO" (talk :talks) "location")
          clj-conj    (project-finder "Clojure/conj, Austin, TX" (talk :talks) "location")
          vcfmw       (project-finder "Accidentally Arming a Hacker Revolution" (talk :talks) "title")
          unlikely    (project-finder "Unlikely Harbingers" (talk :talks) "title")
          i-take      (project-finder "I T.A.K.E Unconference (Keynote), Bucharest, Romania" (talk :talks) "location")
          clj-brdg    (project-finder "ClojureBridge New York City" (talk :talks) "location")
          modes       (project-finder "The Grammar of the Internet" (talk :talks) "title")
          nycdh       (project-finder "New York City Digital Humanities Festival" (talk :talks) "location")
          c-base      (project-finder "Harvesting Human Intelligence" (talk :talks) "title")
          pecha       (project-finder "Computers & Intimacy" (talk :talks) "title")]

      (div {:class "row"}
           (div {:class "col-xs-12 job"}
                (section-header "Selected Public Speaking Experience")
                (two-col-list [curry-on pycon strangeloop anthropology clj-conj i-take clj-brdg] speaking-layout)
                (two-col-list [computation internet modes creative-code sigcis nycdh c-base] speaking-layout))))))

(def talks
  (fn [talk]
    (let [stevens   (project-finder "Stevens Institute of Technology" (talk :talks) "subtitle")
          ai        (project-finder "Illinois Institute of Art" (talk :talks) "subtitle")
          ccc       (project-finder "Columbia College Chicago" (talk :talks) "subtitle")
          iadt      (project-finder "International Academy of Design and Technology" (talk :talks) "subtitle")]

       (div {:class "row"}
            (section-header "Research and Teaching Experience" "experience-section-header")
            (reduce str (clojure.core/map #(line-builder %) [stevens ai iadt ccc]))))))

;;;;;;;;;;;;;;;;;;;;
;; Publications   ;;
;;;;;;;;;;;;;;;;;;;;

(defn pub-layout [pub]
  (li
   (p (strong (pub :title)))
   (p (pub :publication))))

(def publications
  (fn [exhib] (let [pub-list (exhib :publications)
               html-pub ""
               first-2-pubs (take 2 pub-list)
               second-2-pubs (take 2 (drop 2 pub-list))]
           (div {:class "row"}
                (section-header "Selected Publications")
                (two-col-list first-2-pubs pub-layout)
                (two-col-list second-2-pubs pub-layout)))))

;;;;;;;;;;;;;;;;;;;;
;; Sub-Layouts    ;;
;;;;;;;;;;;;;;;;;;;;

(defn cv-sub-layout [data-set]
  (div
   (bio-summary) ;; summary header
   (hr-)

   (experience (data-set :projects) (data-set :employment))
   (hr- )

   (div (public-speaking (data-set :talks)))
   (hr- )

   (publications (data-set :exhibitions))
   (hr-)

   ;; (div {:class "page-breaker"})
   (awards (data-set :exhibitions))
   (hr-)

   (div (talks (data-set :talks)))

   ))

;; Programming

(defn programming-sub-layout [data-set]
  (div
   (bio-summary) ;; summary header

   (experience (data-set :projects) (data-set :employment))
   (small "&nbsp;")

   (div (public-speaking (data-set :talks)))
   (small "&nbsp;")

   (publications (data-set :exhibitions))
   (small "&nbsp;")

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; 3 columns of academic experience
   (academy (data-set :exhibitions) (data-set :talks))

   ))

;; Teaching

(defn teaching-sub-layout [data-set]
  (div

   (talent) ;; alternative header: 3 columns listing talents
   (hr-)

   (div (talks (data-set :talks)))

   ;;(div {:class "page-breaker"})

   (div (public-speaking (data-set :talks)))

   (publications (data-set :exhibitions))
   (hr-)

   (awards (data-set :exhibitions))
   (hr-)

   (academy-horiz (data-set :exhibitions))))

;;;;;;;;;;;;;;;;;;;;
;; Main Layout    ;;
;;;;;;;;;;;;;;;;;;;;

(defn -main
  [& args]

  (let [data-set (get-data)]
  (println
   (html
    (head (title {:style "font-family:'Courier';"} "D. Schm&uuml;dde: Resume")
          (link {:rel "stylesheet" :type "text/css" :href "css/bootstrap.min.css" :media "all"})
          (link {:rel "stylesheet" :href "css/styles.css"})
          (link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
                 :integrity "sha256-3dkvEK0WLHRJ7/Csr0BZjAWxERc5WH7bdeUya2aXxdU= sha512-+L4yy6FRcDGbXJ9mPG8MT/3UCDzwR9gPeyFNMCtInsol++5m3bk2bXWKdZjvybmohrAsn3Ua5x8gfLnbE1YkOg=="
                 :crossorigin "anonymous"}))
    (body
     (div {:class "container" :id "inner"}
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Header: name, title, phone number, etc...
          (resume-header)

          ;;;;;;;;;;;;;;;;;;
          ;; Document Body
          (div {:id "bd"}
               #_(cv-sub-layout data-set)
               (programming-sub-layout data-set)
               #_(teaching-sub-layout data-set)))

     (resume-footer))))))
