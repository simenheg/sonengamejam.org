;; Local Variables:
;; eval: (put 'defentry 'lisp-indent-function 1)
;; End:

;; ------------------------------------------------------------ [ Game jams ]
(defstruct gamejam id date theme)

(defparameter *gamejams*
  (list
   (make-gamejam :id 'oct-14 :date "October 2014" :theme "Limit")
   (make-gamejam :id 'mar-14 :date "March 2014" :theme "Coupled")
   (make-gamejam :id 'sep-13 :date "September 2013" :theme "Geometry")
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
     (melonjs "melonJS" "http://melonjs.org/")
     (iio "iio Engine" "http://iioengine.com/")
     (gamesalad "GameSalad" "http://gamesalad.com/")
     (jquery "jQuery" "http://jquery.com/")
     (blender "Blender" "http://www.blender.org/")
     (pygame "Pygame" "http://www.pygame.org/")
     (qt "Qt" "https://qt-project.org/")
     (javascript "JavaScript" "http://en.wikipedia.org/wiki/Javascript")
     (opengl "OpenGL" "http://www.opengl.org/")
     (libgdx "libgdx" "http://libgdx.badlogicgames.com/")
     (sdl "SDL" "https://www.libsdl.org/")
     (illustrator "Adobe Illustrator" "https://www.adobe.com/products/illustrator/"))))

;; ----------------------------------------------------- [ Download formats ]
(defun format-precedence (format)
  (position format '(:play :love :jar :exe :blender :gamemaker :pdf :github
                     :bitbucket :zip :dmg)))

(defun format< (f1 f2)
  (< (format-precedence f1) (format-precedence f2)))

(defun format-image (format)
  (concatenate
   'string "icons/"
   (ecase format
     (:bitbucket "bitbucket.png")
     (:github "github.png")
     (:jar "jar.png")
     (:zip "zip.png")
     (:love "love.png")
     (:play "play.png")
     (:blender "blender.png")
     (:pdf "pdf.png")
     (:exe "exe.png")
     (:gamemaker "gamemaker.png")
     (:dmg "dmg.png"))))
; NOTE: also add new formats to the format-precedence sequence!

;; -------------------------------------------------------------- [ Entries ]
(defstruct entry
  (team-name "n/a" :type string)
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

    (setf (entry-downloads new-entry) ; keep team member list sorted
          (sort (entry-downloads new-entry) #'format< :key #'first))

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
;; May 2013

(defentry 'may-13
  :team-name "CYBRILL"
  :game-title "The Flood"
  :team-members '("Thor Merlin Lervik")
  :framework 'processing
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/b/b2/game-jam-2013-escape.zip"))
  :screenshot "the-flood-2.png")

(defentry 'may-13
  :team-name "Fat Pixie"
  :game-title "Bob in Chains"
  :team-members '("Kevin Engelsjord"
                  "Peter Rasmussen Lubiana"
                  "Robin Aasen")
  :framework 'gamemaker
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/7/7f/fat_pixie_studios-bob-in-chains-0.1.zip"))
  :rank 2
  :screenshot "bob-in-chains.png")

(defentry 'may-13
  :team-name "Game Jam organizers"
  :game-title "Pig Run"
  :team-members '("Simen Heggestøyl")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/pig-run.love"))
  :screenshot "pig-run.png")

(defentry 'may-13
  :team-name "keep it lol"
  :game-title "Fishly"
  :team-members '("Lorenz Kellerer")
  :framework 'unity
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/c/cb/code_keep_it_lol.zip"))
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
  :downloads '((:zip "https://github.com/LarsBK/sonengamejam/archive/master.zip")
               (:github "https://github.com/LarsBK/sonengamejam/"))
  :screenshot "bobs-silly-adventure-to-france-ii.png")

(defentry 'may-13
  :team-name "No Name, No Game"
  :game-title "The aMAZEing Escape"
  :team-members '("Daniel Rødskog" "Erlend Kristiansen")
  :framework 'processing
  :downloads '((:jar "https://bitbucket.org/HrKristiansen/gamejam2013/downloads/game.jar")
               (:bitbucket "https://bitbucket.org/HrKristiansen/gamejam2013"))
  :screenshot "the-amazeing-escape.png")

(defentry 'may-13
  :team-name "Rancid Applications"
  :game-title "Rancid"
  :team-members '("Martin Stensgård")
  :framework 'amos
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/0/0a/Rancid.zip"))
  :screenshot "rancid.png")

(defentry 'may-13
  :team-name "Team Beam"
  :game-title "Dark Escape"
  :team-members '("Julian Hisdal Nymark"
                  "Kyrre Havik Eriksen")
  :framework 'love
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/2/2f/team-beam-dark-escape.zip")
               (:bitbucket "https://bitbucket.org/beamteam/gamejamgame"))
  :rank 1
  :screenshot "dark-escape.png")

(defentry 'may-13
  :team-name "Team Duckhunt"
  :game-title "The Escape"
  :team-members '("Andreas Orvik" "Ivar Tryti" "Stian Valle")
  :framework 'love
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/0/03/TheEscape.zip"))
  :screenshot "the-escape.png")

(defentry 'may-13
  :team-name "Team Kyrre"
  :game-title "Team Kyrre"
  :team-members '("Asbjørn Lysne Voje"
                  "Halvard Eggen"
                  "Linda Bech"
                  "Tommy Vitikka")
  :framework 'melonjs
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/c/c9/Team_kyrre.zip"))
  :screenshot "team-kyrre.png")

;; September 2013

(defentry 'sep-13
  :team-name "Ευκλείδης"
  :game-title "Grand Theft Athens"
  :team-members '("Simen Heggestøyl"
                  "Thor Merlin Lervik")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/grand-theft-athens.love"))
  :screenshot "gta.png"
  :rank 3)

(defentry 'sep-13
  :team-name "Sine of Madness"
  :game-title "Beware The Angles"
  :team-members '("Bjørn Ivar Teigen"
                  "Vegard Ødegaard"
                  "Vetle Solgaard"
                  "Tønnes Nygaard")
  :framework 'processing
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/3/35/BewareTheAngles.zip"))
  :screenshot "beware-the-angels.png")

(defentry 'sep-13
  :team-name "Faeces Fairy"
  :game-title "BitQuest"
  :team-members '("Julian Hisdal Nymark"
                  "Srod Karim"
                  "Jonathan Ringstad")
  :framework 'love
  :downloads '((:zip "https://docs.google.com/file/d/0B-g6pWxqlbn7S1dPSFJtTzIzVFE/edit?usp=drive_web"))
  :screenshot "bitquest.png"
  :rank 2)

(defentry 'sep-13
  :team-name "BluCoders"
  :game-title "BluBall"
  :team-members '("Håkon Struijk Holmen"
                  "Aleksander Berge Kjellsen")
  :framework 'love
  :downloads '((:love "http://thehawken.org/BluBall.love"))
  :screenshot "bluball.png")

(defentry 'sep-13
  :team-name "Geohazard"
  :game-title "Geohazard"
  :team-members '("Emil Hatlelid"
                  "Kenneth Karadas"
                  "Marie Roald"
                  "Fern Jimeno")
  :framework 'processing
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/3/37/geohazard.zip")
               (:github "https://github.com/ehatle/GAMEJAM2013"))
  :screenshot "geohazard.png")

(defentry 'sep-13
  :team-name "Eplesaft"
  :game-title "EpleTD"
  :team-members '("Jon Ramvi"
                  "Dag Martin Mikkelsen"
                  "Lasse Jul-Larsen")
  :framework 'iio
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/c/cd/epletd.zip")
               (:github "https://github.com/ljl/epletd"))
  :screenshot "epletd.png")

(defentry 'sep-13
  :team-name "Team Permadeath"
  :game-title "Geomancer"
  :team-members '("Kjell Wilhelmsen"
                  "Jan Anders Bremer"
                  "Veronika Heimsbakk"
                  "Arne Hassel"
                  "Pawel Kozlowski")
  :framework 'love
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/b/b3/geomancer.zip")
               (:github "https://github.com/Team-Permadeath/geomancer"))
  :screenshot "geomancer.png")

(defentry 'sep-13
  :team-name "High Risk, High Reward"
  :game-title "Geometrophobia"
  :team-members '("Lukas Wijgaart van Dijk"
                  "Åsmund Dæhlen"
                  "Adrian Eckbo Hoel"
                  "Kristoffer Gudmundsen"
                  "Eivind Furuberg")
  :framework 'gamesalad
  :downloads '((:play "http://arcade.gamesalad.com/game/114562")
               (:zip "http://sonen.ifi.uio.no/w/images/5/5a/geometrophobia.zip"))
  :screenshot "geometrophobia.png")

(defentry 'sep-13
  :team-name "Marky Games"
  :game-title "Geometry Trek III: The Search for SICP"
  :team-members '("Marius Ekeberg")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/geometry-trek-iii.love"))
  :screenshot "geometry-trek-iii.png")

(defentry 'sep-13
  :team-name "Darkside"
  :game-title "NeverSquare"
  :team-members '("Mark Polak"
                  "Raphaela Heil"
                  "Fabian Rosenthal"
                  "Sarah Fullmer"
                  "Robert Kolner"
                  "John Lausund")
  :framework 'jquery
  :downloads '((:play "http://xennis.github.io/NeverSquare/")
               (:zip "http://sonen.ifi.uio.no/w/images/0/0f/neversquare.zip")
               (:bitbucket "https://bitbucket.org/robert_kolner/darkside"))
  :screenshot "neversquare.png")

(defentry 'sep-13
  :team-name "Orbiter"
  :game-title "Orb"
  :team-members '("Karl Magnus Kalvik"
                  "Bård-Kristian Krohg")
  :framework 'blender
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/c/cd/Orb.zip"))
  :screenshot "orb.png"
  :rank 1)

(defentry 'sep-13
  :team-name "Orca"
  :game-title "Lost Hammer of Thor"
  :team-members '("Sina Gholoubi"
                  "Robert Pecserke"
                  "Alba Villalba"
                  "Radka Musilkova"
                  "Saba Sadeghi Rashed")
  :framework 'pygame
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/2/2b/lost-hammer-of-thor.zip"))
  :screenshot "lost-hammer-of-thor.png")

(defentry 'sep-13
  :team-name "Team Beam"
  :game-title "Snake in Space"
  :team-members '("Kyrre Havik Eriksen")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/snakeInSpace.love")
               (:github "https://github.com/Kyrremann/snake-in-space"))
  :screenshot "snake-in-space.png")

(defentry 'sep-13
  :team-name "Peteeer"
  :game-title "The Man Who Wanted to Know"
  :team-members '("Peter Lubiana")
  :framework 'melonjs
  :downloads '((:play "http://folk.uio.no/simenheg/the-man-who-wanted-to-know/")
               (:zip "http://sonen.ifi.uio.no/w/images/7/7a/themanwhowantedtoknow.zip"))
  :screenshot "the-man-who-wanted-to-know.png")

(defentry 'sep-13
  :team-name "Flyvefisk"
  :game-title "Not CraftMine"
  :team-members '("Peter Havgar"
                  "Persijn Kwekkeboom")
  :framework 'processing
  :downloads '((:zip "http://sonen.ifi.uio.no/w/images/c/c5/Not_CraftMine_0_2.zip"))
  :screenshot "not_craftmine.png")

;; March 2014

(defentry 'mar-14
  :team-name "3D-Incorporated"
  :game-title "Devotion"
  :team-members '("Persijn David Kwekkeboom"
                  "Thor Merlin Lervik"
                  "Ådne Lyngstad Nilsen")
  :framework 'blender
  :downloads '((:blender "http://folk.uio.no/simenheg/devotion.blend"))
  :screenshot "devotion.png")

(defentry 'mar-14
  :team-name "Blodgruppa"
  :game-title "Receptor"
  :team-members '("Sigmund Hansen")
  :framework 'unity
  :downloads '((:zip "http://folk.uio.no/simenheg/Receptor.zip"))
  :screenshot "receptor.png")

(defentry 'mar-14
  :team-name "Camel Jockeys"
  :game-title "Forest"
  :team-members '("Mitch Curtis"
                  "Triva Linda M. Shahin"
                  "Ådne Lyngstad Nilsen")
  :framework 'qt
  :downloads '((:zip "http://folk.uio.no/simenheg/gamejam-camel-jockey.zip"))
  :screenshot "forest.png")

(defentry 'mar-14
  :team-name "Faeces Fairy"
  :game-title "Typing Of The LÖVE"
  :team-members '("Julian Nymark"
                  "Srod Karim"
                  "Jonathan Ringstad"
                  "Preben Ø. Aas")
  :framework 'love
  :downloads '((:love "http://virvel.de/%7esind/Typing.love")
               (:bitbucket "https://bitbucket.org/Sind/typing-of-the-l-ve")
               (:zip "http://virvel.de/%7esind/Typing.zip"))
  :screenshot "typing-of-the-love.png")

(defentry 'mar-14
  :team-name "Marky Games"
  :game-title "Animal Operatus"
  :team-members '("Marius Ekeberg"
                  "Marianne Hval")
  :framework 'melonjs
  :downloads '((:play "http://folk.uio.no/simenheg/animal_operatus/")
               (:zip "http://folk.uio.no/simenheg/animal_operatus.zip"))
  :screenshot "animal-operatus.png")

(defentry 'mar-14
  :team-name "Pocket Fluff Entertainment"
  :game-title "Space Whales"
  :team-members '("Solveig Hansen"
                  "Oliver Getz Rodahl"
                  "Christina Lewis")
  :framework 'javascript
  :downloads '((:play "http://folk.uio.no/simenheg/space-whales/")
               (:zip "http://folk.uio.no/simenheg/space-whales.zip"))
  :screenshot "space-whales.png")

(defentry 'mar-14
  :team-name "popkek"
  :game-title "Frappy Bird"
  :team-members '("Simen Heggestøyl"
                  "Ådne Lyngstad Nilsen")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/FrappyBird.love")
               (:github "https://github.com/simenheg/frappy-bird"))
  :screenshot "frappy-bird.png")

(defentry 'mar-14
  :team-name "Rancid Applications"
  :game-title "Rancid Balls"
  :team-members '("Martin Stensgård")
  :framework 'opengl
  :downloads '((:zip "https://github.com/mastensg/rancid2/archive/master.zip")
               (:github "https://github.com/mastensg/rancid2"))
  :screenshot "rancid-balls.png")

(defentry 'mar-14
  :team-name "Team Bob"
  :game-title "Bob's Failed Experiment"
  :team-members '("Lars Bjørlykke Kristiansen"
                  "Aron Jansson Nordberg"
                  "Stine Lønnqvist Skillebæk"
                  "Bjørn-Ingar Bergum")
  :framework 'unity
  :downloads '((:zip "http://folk.uio.no/larsbk/gamejam.zip")
               (:github "https://github.com/LarsBK/Sonen_gamejam2014"))
  :rank 1
  :screenshot "bobs-failed-experiment.png")

(defentry 'mar-14
  :team-name "Vegard Knutsen Lillevoll"
  :game-title "Bobby Hop"
  :team-members '("Vegard Knutsen Lillevoll")
  :framework 'pygame
  :downloads '((:zip "http://folk.uio.no/simenheg/bobby-hop.zip"))
  :rank 2
  :screenshot "bobby-hop.png")

(defentry 'mar-14
  :team-name "Wafflewings"
  :game-title "Lightbringer"
  :team-members '("Karl Magnus Kalvik"
                  "Mathias H. Wilhelmsen"
                  "Bård-Kristian Krohg")
  :framework 'libgdx
  :downloads '((:jar "http://folk.uio.no/simenheg/Lightbringer.jar")
               (:zip "https://github.com/karlma91/gdxgame/archive/master.zip")
               (:github "https://github.com/karlma91/gdxgame"))
  :rank 3
  :screenshot "lightbringer.png")

;; October 2014

(defentry 'oct-14
  :team-name "asdfasdf (this game did not compete)"
  :game-title "Bus driver"
  :team-members '("Julian Nymark")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/bus-driver.love")
               (:bitbucket "https://bitbucket.org/Julianhn/joystick_game/")
               (:zip "https://bitbucket.org/Julianhn/joystick_game/get/master.zip"))
  :screenshot "bus-driver.png")

(defentry 'oct-14
  :team-name "Aventurine Games"
  :game-title "Horizon Escape"
  :team-members '("Hans-Petter Harveg"
                  "Arne Tobias Malkenes Ødegaard"
                  "Lars Musland"
                  "Espen Haukeland Kristensen")
  :framework 'unity
  :downloads '((:zip "https://www.dropbox.com/s/uj8ju8ffby9hqd3/Horizon%20Escape.zip?dl=1"))
  :screenshot "horizon-escape.png")

(defentry 'oct-14
  :team-name "Veronika"
  :game-title "Circuits"
  :team-members '("Veronika Heimsbakk")
  :framework 'illustrator
  :downloads '((:pdf "http://folk.uio.no/simenheg/circuits.pdf")
               (:zip "http://folk.uio.no/simenheg/circuits.zip"))
  :screenshot "circuits.png")

(defentry 'oct-14
  :team-name "Techum"
  :game-title "That one bottle"
  :team-members '("Steffen Nilsen"
                  "Elena Betora")
  :framework 'libgdx
  :downloads '((:jar "http://folk.uio.no/simenheg/thatonebottle.jar")
               (:zip "https://github.com/myrsnipe/alchemylab/archive/master.zip")
               (:github "https://github.com/myrsnipe/alchemylab"))
  :screenshot "that-one-bottle.png")

(defentry 'oct-14
  :team-name "Trop Bien"
  :game-title "IFI-Plattformer"
  ;; TODO: Get full team-member names
  :team-members '("Torbjørn"
                  "Benjamin")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/ifi-plattformer.love")
               (:zip "http://folk.uio.no/simenheg/ifi-plattformer.zip"))
  :screenshot "ifi-plattformer.png")

(defentry 'oct-14
  :team-name "Team Erik"
  :game-title "LIMIT"
  :team-members '("Erik Nordstrøm")
  :framework 'sdl
  :downloads '((:github "https://github.com/erikano/2014-10-sonen-game-jam")
               (:zip "https://github.com/erikano/2014-10-sonen-game-jam/archive/master.zip"))
  :screenshot "limit.png")

(defentry 'oct-14
  :team-name "Robots in therapy"
  :game-title "Toxic Business"
  :team-members '("Haakon Drews"
                  "Thor Merlin Lervik")
  :framework 'pygame
  :downloads '((:bitbucket "https://bitbucket.org/rohtie/gamejam-autumn2014")
               (:zip "https://bitbucket.org/rohtie/gamejam-autumn2014/get/master.zip"))
  :screenshot "toxic-business.png")

(defentry 'oct-14
  :team-name "Team Beam"
  :game-title "LittleBigSpaceship"
  :team-members '("Kyrre Havik Eriksen")
  :framework 'love
  :downloads '((:love "http://2k3.org/forum/LittleBigSpaceship.love")
               (:exe "http://www.2k3.org/wp-content/uploads/Little-Big-Spaceship.zip")
               (:github "https://github.com/Kyrremann/LittleBigSpaceship")
               (:zip "https://github.com/Kyrremann/LittleBigSpaceship/archive/master.zip"))
  :rank 3
  :screenshot "littlebigspaceship.png")

(defentry 'oct-14
  :team-name "Team Øyvind"
  :game-title "Limit Virus"
  :team-members '("Øyvind J. Amundrud")
  :framework 'libgdx
  :downloads '((:jar "http://folk.uio.no/simenheg/limit-virus.jar")
               (:github "https://github.com/Snovind/gamejam")
               (:zip "https://github.com/Snovind/gamejam/archive/master.zip"))
  :screenshot "limit-virus.png")

(defentry 'oct-14
  :team-name "Team Blox"
  :game-title "Airs up"
  :team-members '("Endre Svensen"
                  "Jarle Sandnes"
                  "Lise T. Nilsen"
                  "Gunnar Holst")
  :framework 'unity
  :downloads '((:github "https://github.com/Sanjar14/GameJam2")
               (:zip "https://github.com/Sanjar14/GameJam2/archive/master.zip"))
  :screenshot "airs-up.png")

(defentry 'oct-14
  :team-name "Terje Ballestad"
  :game-title "Jump the wall!"
  :team-members '("Terje Ballestad")
  :framework 'gamemaker
  :downloads '((:gamemaker "http://folk.uio.no/simenheg/jump-the-wall.gmx")
	       (:zip "http://virvel.de/jump-the-wall.zip"))
  :screenshot "jump-the-wall.png")

(defentry 'oct-14
  :team-name "Roger.dat"
  :game-title "Sugar Shock: Limited Edition"
  :team-members '("Morten Hillbom"
                  "Joakim Lier"
                  "Bendik Østrem Svalastog"
                  "Emily Lo"
                  "Karl Hole Totland"
                  "Steinar Pedersen")
  :framework 'love
  :downloads '((:love "http://folk.uio.no/simenheg/SugarShock.love")
               (:zip "http://folk.uio.no/simenheg/SugarShock.zip"))
  :screenshot "sugar-shock.png")

(defentry 'oct-14
  :team-name "TBD"
  :game-title "Pixelhipster"
  :team-members '("Jens Bache-Wiig"
                  "Lars Moen")
  :framework 'qt
  :downloads '((:zip "http://folk.uio.no/simenheg/pixelhipster.zip")
	       (:dmg "http://virvel.de/pixelhipster.dmg"))
  :rank 1
  :screenshot "pixelhipster.png")

(defentry 'oct-14
  :team-name "The Icelandics"
  :game-title "Lofsongur"
  :team-members '("Ingrid Dæhlen"
                  "Snorre Fredriksen Hofstad"
                  "Kristine Hein")
  :framework 'pygame
  :downloads '((:zip "http://folk.uio.no/simenheg/lofsongur.zip"))
  :screenshot "lofsongur.png")

(defentry 'oct-14
  :team-name "Fluffy Bunny"
  :game-title "Hamster Simulator"
  :team-members '("Kristoffer Gudmundsen"
                  "Gosia Kokoszka")
  :framework 'unity
  :downloads '(;(:exe "https://www.dropbox.com/sh/f7yac4mtur69o6p/AABXKXLa_RfQQayzSqlg6YG9a/Hamster_Simulator.exe?dl=1")
	       (:zip "http://virvel.de/hamster-simulator.zip"))
  :screenshot "hamster-simulator.png")

(defentry 'oct-14
  :team-name "Team Walkover"
  :game-title "LIMIT Release"
  :team-members '("Terje Kirstihagen"
                  "Trym Horgen")
  :framework 'gamemaker
  :downloads '((:exe "http://folk.uio.no/simenheg/limit-release.1.01.exe")
               (:gamemaker "http://folk.uio.no/simenheg/limit-release.1.01.gmk")
	       (:gamemaker "http://virvel.de/limit-release.gb1")
	       )
  :rank 2
  :screenshot "limit-release.png")

(defentry 'oct-14
  :team-name "UNITY"
  :game-title "Orange Blob"
  :team-members '("Persijn Kwekkeboom")
  :framework 'unity
  :downloads '((:zip "http://folk.uio.no/simenheg/orange-blob.zip"))
  :screenshot "orange-blob.png")

(defentry 'oct-14
  :team-name "Wafflewings"
  :game-title "Flash Fever"
  :team-members '("Karl Magnus Kalvik"
                  "Bård-Kristian Krohg")
  :framework 'libgdx
  :downloads '((:jar "http://folk.uio.no/simenheg/FlashFever.jar")
               (:github "https://github.com/karlma91/GameJamLimit"))
  :screenshot "flash-fever.png")

(defentry 'oct-14
  :team-name "Ole Herman Schumacher Elgesem"
  :game-title "Unfinished space game prototype with long name"
  :team-members '("Ole Herman Schumacher Elgesem")
  :framework 'sdl
  :downloads '(
	       (:zip "http://virvel.de/unfinished-space-game-prototype.zip")
	       (:github "https://github.com/olehermanse/SGP")
	       )
  :screenshot "unfinished-space-game.png")

(defentry 'oct-14
  :team-name "Team AwesomePants"
  :game-title "AwesomePants Flightschool"
  :team-members '("Nikolas Papaioannou"
                  "Vegard Knutsen Lillevoll"
                  "Eskil Opdahl Nordland")
  :framework 'pygame
  :downloads '((:zip "http://folk.uio.no/simenheg/awesomepantsflightschool.zip"))
  :screenshot "flightschool.png")

(defentry 'oct-14
  :team-name "High Risk"
  :game-title "RGB-run"
  :team-members '("Odd Gunnar Fatland"
                  "Eivind Furuberg"
                  "Lukas Wijgart")
  :framework 'unity
  :downloads '((:zip "http://virvel.de/rgb.zip"))
  :screenshot "rgb-run.png")
