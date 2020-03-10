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

#_(defn find-project [keyword-term search-term db]
  (if (= ((first db) keyword-term) search-term)
    (first db)
    (find-project keyword-term search-term (rest db))))

(defn find-project [keyword-term search-term db]
  (cond
    (empty? db) nil
    (= ((first db) keyword-term) search-term) (first db)
    :else (find-project keyword-term search-term (rest db))))

;; (find-project :title "2018 Researcher in Residence" (:awards (x :exhibitions)))

;; (find-project :title "2017 Researcher in Residence" (:awards (x :exhibitions)))

;;;;;;;;;;;;;;;;;;;;
;; Static Layouts ;;
;;;;;;;;;;;;;;;;;;;;

(defn resume-header []
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
                                   "&nbsp;" (bio/biography :city))))))))

(defn resume-footer []
  (footer {:class "footer"}
          (div {:class "container"}
               (p "Made With Clojure &nbsp;|&nbsp; See the Source Code: "
                  (a {:href "http://bit.ly/2bWWDHc"} "http://bit.ly/2bWWDHc")))))

;;;;;;;;;;;;;;;;;;;;
;; Layouts        ;;
;;;;;;;;;;;;;;;;;;;;

(defn section-header [header-name & additional-class]
  (div {:class "col-xs-12"}
       (h2 {:class (str "text-center section-header " (first additional-class))} header-name)))

(defn make-list [description]
  "I create a HTML list. I expect a string of data delimited by semicolons."
  (ul {:class "qualifications"} (desc-itemizer (re-matcher #"[^~]+" description) "")))

(defn three-column-layout [talent title description]
  "I take parameters and build a single entry"
  (div {:class "col-xs-4"}
       (h2 talent)
       (h3 title)
       (p description)))

(defn two-column-layout [subtitle title technology]
  "I take parameters and build a single entry"
  (div {:class "col-xs-6 text-center"}
       (h2 subtitle)
       (h3 title)
       (p technology)))

(defn append-category [category]
  (if category
    (str "TECHNOLOGIES: " category)
    category))

(defn single-column-layout
  "I take parameters and build a single entry"
  [{:keys [title date desc subtitle technology synopsis]}]
  (div {:class "col-xs-12"}
       (h2 title (small {:class "pull-right"} date))
       (h3 subtitle (small (append-category technology)))
       (p desc)
       (if (nil? synopsis) nil (make-list synopsis))))

(defn list-achievements [achievements html-out template]
  (if (seq achievements)
    (list-achievements (rest achievements)
                       (str html-out (template (first achievements)))
                       template)
    html-out))

(defn two-col-list-layout
  "I take a list and a layout and build out an entire column"
  [template list-column]
  (div {:class "two-col-list"}
       (ul {:class "list-unstyled"}
           (list-achievements list-column  "" template))))

;; TODO: simplify model
(defn speaking-publication-award-layout
  "Layout for awards, publications, and speaking engagements. Handles data variations.
   ---
   `location`: for speaking engagements OR
   `publication`: for publications OR
   `org`: for awards
   ---
   `date`: date of event/publication OR
   `type`: type of award"
  [{:keys [title date location publication org type]}]
  (li
   (div (strong title))
   (div (or location publication org))
   (div (small {:class "date"} (or date type)))))

;;;;;;;;;;;;;;;;;;;;
;; Talent Header  ;;
;;;;;;;;;;;;;;;;;;;;

(defn talent-column [talent]
  (three-column-layout ((bio/talent (keyword talent)) :title)
                       ((bio/talent (keyword talent)) :domain)
                       ((bio/talent (keyword talent)) :desc)))

(defn talent-layout []
  (div {:class "row"}
       (talent-column "educator")
       (talent-column "programmer")
       (talent-column "creative")))

(defn summary-layout []
  (div {:class "row"}
       (section-header "Summary")
       (div {:class "col-xs-12"}
            (p (bio/biography :summary)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Selected Items From a Category   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-selected-awards [exhib] exhib)


(defn experience [employ]
  (let [db           (employ :employers)
        btf          (find-project :title "Beyond the Frame" db)
        nextjournal  (find-project :title "Nextjournal" db)
        penguin      (find-project :title "Penguin Random House" db)
        netgalley    (find-project :title "NetGalley" db)
        ef-sharp     (find-project :title "F#" db)]

    (div {:class "row"}
         (section-header "Experience" "experience-section-header")
         (reduce str (clojure.core/map #(single-column-layout %) [nextjournal btf penguin netgalley ef-sharp])))))

;;;;;;;;;;;;;;;;;;;;
;; Academic       ;;
;;;;;;;;;;;;;;;;;;;;

(defn public-speaking [talk]
  (let [db (talk :talks)
        curry-on    (find-project :location "Curry On, London, England" db)
        pycon       (find-project :location "PyCon/PyData, Berlin, Germany" db)
        computation (find-project :location "Society for the History of Technology, Milan, Italy" db)
        internet    (find-project :location "New York City" db)
        strangeloop (find-project :location "Strange Loop, St. Louis, MO" db)
        creative-code (find-project :location "Creative Coding NYC" db)
        anthropology  (find-project :title "Enabling Digital Anthropology" db)
        intended      (find-project :title "Intended Knowledge?" db)
        sigcis      (find-project :location "Stored In Memory: The 10th Annual SIGCIS Conference, St. Louis, MO" db)
        clj-conj    (find-project :location "Clojure/conj, Austin, TX" db)
        vcfmw       (find-project :title "Accidentally Arming a Hacker Revolution" db)
        unlikely    (find-project :title "Unlikely Harbingers" db)
        i-take      (find-project :location "I T.A.K.E Unconference (Keynote), Bucharest, Romania" db)
        clj-brdg    (find-project :location "ClojureBridge New York City" db)
        nycdh       (find-project :location "New York City Digital Humanities Festival" db)
        modes       (find-project :title "The Grammar of the Internet" db)
        c-base      (find-project :title "Harvesting Human Intelligence" db)
        pecha       (find-project :title "Computers & Intimacy" db)]

    (div {:class "row"}
         (section-header "Selected Public Speaking Experience")
         (two-col-list-layout speaking-publication-award-layout [curry-on pycon strangeloop anthropology clj-conj i-take clj-brdg])
         (two-col-list-layout speaking-publication-award-layout [computation internet modes creative-code sigcis nycdh c-base]))))

(defn academy-horiz [exhib]
  (let [db (exhib :education)
        nu (find-project :subtitle "Northwestern University" db)
        uni (find-project :subtitle "University of Northern Iowa" db)]
    (div {:class "row"}
         (section-header "Education")
         (two-column-layout (nu :subtitle) (nu :title) (nu :technology))
         (two-column-layout (uni :subtitle) (uni :title) (uni :technology)))))

(defn teaching [talk]
  (let [db      (talk :talks)
        stevens (find-project :title "Stevens Institute of Technology" db)
        ai      (find-project :title "Illinois Institute of Art" db)
        ccc     (find-project :title "Columbia College Chicago" db)
        iadt    (find-project :title "International Academy of Design and Technology" db)]
    (div {:class "row"}
         (section-header "Research and Teaching Experience" "experience-section-header")
         (reduce str (clojure.core/map #(single-column-layout %) [stevens ai iadt ccc])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get All Items From a Category   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-all-cv-items
  "Get all cv items and put them in two columns. If there is an odd number, `take` and `drop` always round up, so the first column gets the extra item"
  [cv-items layout-style]
  (let [num-of-cv-items (count cv-items)
        one-column (partial two-col-list-layout layout-style)]
    (div
     (one-column (take (/ num-of-cv-items 2) cv-items))
     (one-column (drop (/ num-of-cv-items 2) cv-items)))))

(defn get-all-publications [{:keys [publications]}]
  (div {:class "row"}
       (section-header "Publications")
       (get-all-cv-items publications speaking-publication-award-layout)))

(defn get-all-awards [{:keys [awards]}]
  (div {:class "row"}
       (section-header "Awards")
       (get-all-cv-items awards speaking-publication-award-layout)))

;;;;;;;;;;;;;;;;;;;;
;; Sub-Layouts    ;;
;;;;;;;;;;;;;;;;;;;;

;; Programming

(defn programming-sub-layout [data-set]
  (div
   (summary-layout) ;; summary header

   (experience (data-set :employment))
   (small "&nbsp;")

   (teaching (data-set :talks))

   (public-speaking (data-set :talks))
   (small "&nbsp;")

   (get-all-publications (data-set :exhibitions))
   (small "&nbsp;")

   (get-all-awards (data-set :exhibitions))
   (small "&nbsp;")

   (academy-horiz (data-set :exhibitions))

   ))

;; Teaching

(defn teaching-sub-layout [data-set]
  (div

   (talent-layout) ;; alternative header: 3 columns listing talents
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
    (head (title {:style "font-family:'Courier';"} "David Schmudde: Resume")
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
               (programming-sub-layout data-set)
               #_(teaching-sub-layout data-set)))

     (resume-footer))))))
