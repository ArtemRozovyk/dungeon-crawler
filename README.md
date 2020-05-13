# DungeonCrawling
# dungeon_crawl-


Le points importants du travaill realisé:
-la carte est parsé depuis un fichier 
-N mobs sont placés sur la carte, dans les cases vides, de manière aléatoire (N reglable dans initGameState:State.hs:186)
-Les mobs sont non-réactifs: il regardent autour et constituent une liste des actions possibles et associent un point (aléatoire pour l'instant, 
	prédéfini pour certaines actions (frapper joueur sera implementé et représentera une action plus prioritaire) 
-Un trésor et un piege sont placés aléatoirement dans une case vide sur la carte 
-Une action est choisie aléatoirement, en considerant les poids des actions. c.f. pickRandomWeighted:Model:127
-Le mouvement (génération puis choix d'action puis mouvement) est fait toutes les 3 secondes en considerant le temps courrant. 
	(Important: les instant de départ de chaque mob sont differents, une valeur de cet instant leurs est associé à la création, ceci pour donner une
	 impression des mobs ayant leur propre volonté)
-Le joueur se déplace avec les touches Z S Q D pour les 4 directions et R pour frapper un mob ou prendre un objet.
-un mob frapé 4 fois meurt. 

-Les portes s'ouvrent avec la touche E 

-On gagne quand on arrive à la sortie apès avoir récupéré le trésor (!) 
-On perd quand on marche sur le piege


 



CARTE

156 prop_carte1:: Carte -> Bool  --Vérifie que toutes les cases soient bien comprisent entre la hauteur et la largeur
176 prop_carte2:: Carte -> Bool -- chaque case de la grille contient quelque chose 
186 prop_carte3 :: Carte -> Bool  -- il y a une unique entree et unique sortie
202 prop_carte4 :: Carte -> Bool -- Elle est entièrement entouré des murs 
217 prop_carte5 :: Carte -> Bool --Chaque porte est encadre des murs
263 prop_carte6 :: Carte -> Bool --La sortie est accesible depuis l'entree 

ENVI 

32 prop_envi_inv :: Envi -> Bool  -- Vérifie que l'environnement est saint
36 prop_envi_inv1 :: Envi -> Bool   -- Vérifie qu'il n'y ait pas 2 entités au meêm endroit 
48 prop_envi_inv2 :: Envi -> Bool  --Vérifie qu'il n'y ait pas plus d'un seul joueur
64 prop_envi_inv3 :: Envi -> Bool -- Vérifie que toutes les coordonnés > 0 
109 pre_rmv_envi :: Coord -> Envi -> Bool  --Vérifie qu' il y a bien une entité dans l'envorionnement a ces coordonnées
119 post_rmv_envi :: Coord -> Envi -> Bool  --Vérifie qu'il n'y ait plus d'entités à ces coordonnées
145 post_getPlayer :: (Coord, [Entite]) -> Bool  --Vérifie qu'on ait bien retourné un Joueur
153 pre_ajout_env :: (Coord,Entite) -> Envi -> Bool  --Vérifie qu'il n'y a pas déjà un mob a ces coordonnés

MODELE 

29 prop_modele_inv1 :: Modele -> Bool --Vérifie que les entités sont bien sur une case traversable
41 prop_modele_inv2 :: Modele -> Bool --Vérifie que les entités sont bien dans les limites de la carte
67 prop_modele_inv4 :: Modele -> Bool --Vérifie si le trésor est accesible depuis l'entrée.
187 pre_moveGenerique :: Modele -> Coord -> Bool --On vérifie si les coordonnés sont toujours sur la carte
195 post_moveGenerique :: Modele -> Coord -> Bool --On vérifie si le joueur à bien fini sur ces coordonnés

ETAT 

41 prop_state_inv1 :: Etat -> Bool  --numTour est un entier naturel
45 prop_state_inv2 :: Etat -> Bool --Chaque coordonnée d'entite correspond à une case traversable
57 prop_state_inv3 :: Etat -> Bool   --Les entités sont bien dans les limites de la carte 
69 prop_state_inv4 :: Etat -> Bool --Les mobs ne sont pas l'un sur l'autre (ni sur le joueur) pas de plusieurs entites dans une case
86 prop_pre_add_entity_state1 :: Etat -> Coord -> Entite-> Bool -- Les coordonnées sont comprises dans les limites de la carte
90 prop_pre_add_entity_state2 :: Etat -> Coord -> Entite-> Bool -- Vérifie qu'il n'y a pas d'entités présente sur la case
115 prop_pre_add_entity_state3 :: Etat -> Coord -> Entite -> Bool -- le nombre de mobs sur la carte ne peut pas depasser la moitié des cases vides de la carte(trop peuplé sinon)
121 prop_pre_add_entity_state4 :: Etat -> Coord -> Entite -> Bool -- on ne peut pas ajouter plus d'un joueur 
135 prop_post_add_entity_state :: Etat -> Coord -> Entite -> Bool --l'entite a bien été rajouté

à venir : 

Carte: 
post_openDoor m c = undefined --verifier la que la porte est bien ouverte  
invariant_atteindre_la_sorite --un pathfinder via monad list 

Modele:
post_prevoit m c = undefined --il y a bien des action proposés (dans une situation correcte) - on regarde autours 
post_bouge m e l = undefined --on bouge bien dans une des directions proposés via "decide"






































