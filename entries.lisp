;; Local Variables:
;; eval: (put 'defentry 'lisp-indent-function 1)
;; End:

;; ------------------------------------------------------------ [ Game jams ]
(defstruct gamejam id date theme)

(defparameter *gamejams*
  (list
   (make-gamejam :id 'may-13 :date "May 2013" :theme "Escape")))

;; ----------------------------------------------------------- [ Frameworks ]
(defstruct (framework (:constructor make-framework (name written url)))
  name written url)

(defparameter *known-frameworks*
  (mapcar
   (lambda (fw) (apply #'make-framework fw))
   '((processing "Processing" "http://processing.org/")
     (gamemaker "GameMaker" "http://www.yoyogames.com/")
     (love "LÖVE" "http://love2d.org/")
     (unity "Unity" "http://unity3d.com/")
     (cocos2d "cocos2d" "http://cocos2d.org/")
     (amos "AMOS" "https://en.wikipedia.org/wiki/AMOS_%28programming_language%29")
     (melonjs "melonJS" "http://melonjs.org/"))))

;; ----------------------------------------------------- [ Download formats ]
(defun format-image (format)
  (concatenate
   'string "icons/"
   (ecase format
     (:bitbucket "bitbucket.png")
     (:github "github.png")
     (:jar "jar.png")
     (:zip "zip.png"))))

;; -------------------------------------------------------------- [ Entries ]
(defstruct entry
  (team-name "anonymous" :type string)
  (game-title "n/a" :type string)
  (team-members '() :type list)
  (framework nil)
  (downloads '() :type list) ; a plist in form (:format url)
  (rank nil)
  (screenshot nil))

(defparameter *entries* (make-hash-table))

(defun defentry (gamejam-id &rest args)
  (let ((new-entry (apply #'make-entry args)))
    (setf (entry-team-members new-entry) ; keep team member list sorted
          (sort (entry-team-members new-entry) #'string<))

    (when-let ((framework-name (entry-framework new-entry)))
      (let ((framework (find framework-name *known-frameworks*
                             :key #'framework-name)))
        (setf (entry-framework new-entry)
              (or framework (make-framework nil "n/a" "#")))))

    (pushnew new-entry (gethash gamejam-id *entries*)
             :key #'entry-team-name))) ; only one entry per team

(defun rank-icon (rank)
  (concatenate
   'string "icons/"
   (ecase rank
     (1 "gold-medal.png")
     (2 "silver-medal.png")
     (3 "bronze-medal.png"))))

;; ------------------------------------------------------- [ Define entries ]
(defentry 'may-13
  :team-name "CYBRILL"
  :game-title "The Flood"
  :team-members '("Thor Merlin Lervik")
  :framework 'processing
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/b/b2/game-jam-2013-escape.zip")
  :screenshot "the-flood-2.png")

(defentry 'may-13
  :team-name "Fat Pixie"
  :game-title "Bob in Chains"
  :team-members '("Kevin Engelsjord"
                  "Peter Rasmussen Lubiana"
                  "Robin Aasen")
  :framework 'gamemaker
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/7/7f/fat_pixie_studios-bob-in-chains-0.1.zip")
  :rank 2
  :screenshot "bob-in-chains.png")

(defentry 'may-13
  :team-name "Game Jam organizers"
  :game-title "Pig Run"
  :team-members '("Simen Heggestøyl")
  :framework 'love
  :downloads '(:zip "http://folk.uio.no/simenheg/pig-run.love")
  :screenshot "pig-run.png")

(defentry 'may-13
  :team-name "keep it lol"
  :game-title "Fishly"
  :team-members '("Lorenz Kellerer")
  :framework 'unity
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/c/cb/code_keep_it_lol.zip")
  :rank 3)

(defentry 'may-13
  :team-name "Níðhǫggr"
  :game-title "Bob's Silly Adventure to France II"
  :team-members '("Aleksi Miikkael Luukkonen"
                  "Aron Jansson Nordberg"
                  "Bjørn-Ingar Bergum"
                  "Gorm Lundestad"
                  "Lars Bjørlykke Kristiansen"
                  "Stine Skillebek")
  :framework 'cocos2d
  :downloads '(:zip "https://github.com/LarsBK/sonengamejam/archive/master.zip"
               :github "https://github.com/LarsBK/sonengamejam/")
  :screenshot "bobs-silly-adventure-to-france-ii.png")

(defentry 'may-13
  :team-name "No Name, No Game"
  :game-title "The aMAZEing Escape"
  :team-members '("Daniel Rødskog" "Erlend Kristiansen")
  :framework 'processing
  :downloads '(:jar "https://bitbucket.org/HrKristiansen/gamejam2013/downloads/game.jar"
               :bitbucket "https://bitbucket.org/HrKristiansen/gamejam2013")
  :screenshot "the-amazeing-escape.png")

(defentry 'may-13
  :team-name "Rancid Applications"
  :game-title "Rancid"
  :team-members '("Martin Stensgård")
  :framework 'amos
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/0/0a/Rancid.zip"))

(defentry 'may-13
  :team-name "Team Beam"
  :game-title "Dark Escape"
  :team-members '("Julian Hisdal Nymark"
                  "Kyrre Havik Eriksen")
  :framework 'love
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/2/2f/team-beam-dark-escape.zip"
               :bitbucket "https://bitbucket.org/beamteam/gamejamgame")
  :rank 1
  :screenshot "dark-escape.png")

(defentry 'may-13
  :team-name "Team Duckhunt"
  :game-title "The Escape"
  :team-members '("Andreas Orvik" "Ivar Tryti" "Stian Valle")
  :framework 'love
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/0/03/TheEscape.zip")
  :screenshot "the-escape.png")

(defentry 'may-13
  :team-name "Team Kyrre"
  :team-members '("Asbjørn Lysne Voje"
                  "Halvard Eggen"
                  "Linda Bech"
                  "Tommy Vitikka")
  :framework 'melonjs
  :downloads '(:zip "http://sonen.ifi.uio.no/w/images/c/c9/Team_kyrre.zip"))
