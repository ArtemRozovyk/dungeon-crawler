# DungeonCrawling


Les INVARIANTS sont décrits dans le fichier "invariants.txt"


Le points importants du travaill realisé:

-la carte est parsé depuis un fichier 

-N mobs sont placés sur la carte, dans les cases vides, de manière aléatoire (N reglable dans initGameState:State.hs:186)

-Les mobs sont non-réactifs: il regardent autour et constituent une liste des actions possibles et associent un point (aléatoire pour l'instant, 
	prédéfini pour certaines actions (frapper joueur sera implementé et représentera une action plus prioritaire) 

-Un trésor et un piege sont placés aléatoirement dans une case vide sur la carte 

-Une action est choisie aléatoirement, en considerant les poids des actions. c.f. pickRandomWeighted:Model:127

-Le mouvement (génération puis choix d'action puis mouvement) est fait toutes les 3 secondes en considerant le temps courrant. 
(Important: les instant de départ de chaque mob sont differents, une valeur de cet instant leurs est associé à la création, ceci pour donner une impression des mobs ayant leur propre volonté)

-Le joueur se déplace avec les touches Z S Q D pour les 4 directions et R pour frapper un mob ou prendre un objet.

-un mob frapé 5 fois meurt. 

-Les portes s'ouvrent avec la touche E 

-On gagne quand on arrive à la sortie apès avoir récupéré le trésor (!) 

-On perd quand on marche sur le piege

-Esc pour echapper



 





































