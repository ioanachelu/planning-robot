;functie care intoace culoare bilelor care trebuie bagate in casa
(define the_color
  (lambda (goal)
    (car (cdr goal))
    )
  )
;functie care intoarce locatia casei in care trebuie aduse bilele
(define the_loc
  (lambda  (goal)
    (car (cdr (cdr goal)))
    )
  )
;functie care intoarce locatia curenta
(define my_loc
  (lambda (world)
    (if (equal? (car (car world)) 'Location)
        (car (cdr (car world)))
        (my_loc (cdr world))
        )
    )
  )
;functie care verifica daca locatia curenta este cea finala
(define final_loc
  (lambda (world goal)
    (if (equal? (my_loc world) (the_loc goal))
        #t
        #f
        )
    )
  )
;functie care verifica daca robotul cara bila cautata 
(define carry_the_ball
  (lambda (world goal)
    (if (equal? (car (car world)) 'Carries)
        (if (equal? (car (cdr (car world))) (the_color goal))
            (if (= (car (cdr (cdr (car world)))) 0)
                #f
                #t
                )
            (carry_the_ball (cdr world) goal)
            )
        (carry_the_ball (cdr world) goal)
        )
    )
  )
;functie care verifica daca robotul care o bila
(define carry_sth_else
  (lambda (world goal)
    (if (null? world)
        #f
        (if (equal? (car (car world)) 'Carries)
            (if (= (car (cdr (cdr (car world)))) 0)
                (carry_sth_else (cdr world) goal)
                #t
                )
            (carry_sth_else (cdr world) goal)
            )
        )
    )
  )
;functie care verifica daca robotul cara o bila si intoarce culoarea bilei
(define carry_sth_else2
  (lambda (world goal)
    (if (null? world)
        #f
        (if (equal? (car (car world)) 'Carries)
            (if (= (car (cdr (cdr (car world)))) 0)
                (carry_sth_else2 (cdr world) goal)
                (car (cdr (car world)))
                )
            (carry_sth_else2 (cdr world) goal)
            )
        )
    )
  )
;functie care intoarce numarul de bile carate
(define get_all_carried_balls
  (lambda (world)
    (if (null? world)
        0
        (if (equal? (car (car world)) 'Carries)
            (+ (car (cdr (cdr (car world)))) (get_all_carried_balls (cdr world)))
            (get_all_carried_balls (cdr world))
            )
        )
    )
  )
;functie care verifica daca robotul se poate misca
(define can_move
  (lambda (world)
    (if (= (remainder (get_all_carried_balls world) 2) 0)
        #t
        #f
        )
    )
  )
;functie care verifica daca in locatie curenta se gaseste bila cautata
(define loc_has_the_ball
  (lambda (world goal)
    (if (null? world)
        #f
        (if (equal? (car (car world)) 'Spheres) 
            (if (and (and (equal? (car (cdr (cdr (car world)))) (my_loc world)) (equal? (car (cdr (car world))) (the_color goal))) (not (= (car (cdr (cdr (cdr (car world))))) 0)))
                #t
                (loc_has_the_ball (cdr world) goal)
                )
            (loc_has_the_ball (cdr world) goal)
            )
        )
    )
  )
;functie care verifica daca in locatia curenta se mai gasesc si alte bile in afara de o bila cautata si intoarce culoarea bilei pe care o gaseste
(define loc_has_more_balls
  (lambda (world goal)
    (if (null? world)
        #f
        (if (equal? (car (car world)) 'Spheres) 
            (if (and (and (equal? (car (cdr (cdr (car world)))) (my_loc world)) (equal? (car (cdr (car world))) (the_color goal))) (> (car (cdr (cdr (cdr (car world))))) 1))
                (the_color goal)
                (if (and (and (equal? (car (cdr (cdr (car world)))) (my_loc world)) (not (equal? (car (cdr (car world))) (the_color goal)))) (> (car (cdr (cdr (cdr (car world))))) 0))
                    (car (cdr (car world)))
                    (loc_has_more_balls (cdr world) goal)
                    )
                )
            (loc_has_more_balls (cdr world) goal)
            )
        )
    )
  )
;functie care verifica daca in locatia curenta se gaseste cel putin o bila diferita de bila cautata
(define loc_has_more_balls_any
  (lambda (world goal)
    (if (null? world)
        #f
        (if (equal? (car (car world)) 'Spheres) 
            (if (and (and (equal? (car (cdr (cdr (car world)))) (my_loc world)) (not (equal? (car (cdr (car world))) (the_color goal)))) (> (car (cdr (cdr (cdr (car world))))) 0))
                (if (and (or  (equal? (my_loc world) 'RedWarehouse) (equal? (my_loc world) 'BlueWarehouse)) (or (equal? (car (cdr (car world))) 'Red) (equal? (car (cdr (car world))) 'Blue)))
                    (loc_has_more_balls_any (cdr world) goal)
                    (car (cdr (car world)))
                    )
                (loc_has_more_balls_any (cdr world) goal)
                )  
            (loc_has_more_balls_any (cdr world) goal)
            )
        )
    )
  )
;functie care verifica daca robotul cara deja 2 bile
(define robot_full
  (lambda (world)
    (if (= (get_all_carried_balls world) 2)
        #t
        #f
        )
    )
  )
;functie care face diferenta intre o mult si un element
(define minus-el
  (lambda (el mult)
    (if (null? mult)
        '()
        (if (equal? el (car mult))
            (minus-el el (cdr mult))
            (cons (car mult) (minus-el el (cdr mult)))
            )
        )
    )
  )
;functie care face diferenta a doua multimi                    
(define minus
  (lambda (mult2 mult1)
    (if (null? mult1)
        mult2
        (minus  (minus-el (car mult1) mult2) (cdr mult1))
        )
    )
  )
;functie care intoarce vecinii unui nod
(define get_neighbours
  (lambda (world)
    (if (null? world)
        '() 
        (if (equal? (car (car world)) 'Door)
            (if (equal? (car (cdr (car world))) (my_loc world))
                (cons (car (cdr (cdr (car world)))) (get_neighbours (cdr world)))
                (get_neighbours (cdr world))
                )
            (get_neighbours (cdr world))
            )
        )
    )
  )
;functie care verifica daca robotul e gol
(define robot_empty
  (lambda (world)
    (if (= (get_all_carried_balls world) 0)
        #t
        #f
        )
    )
  )
;functie care intoarce culoarea bilei carate
(define get_carried_ball
  (lambda (world)
    (if (equal? (car (car world)) 'Carries)
        (if (> (car (cdr (cdr (car world)))) 0)
            (car (cdr (car world)))
            (get_carried_ball (cdr world))
            )
        (get_carried_ball (cdr world))
        )
    )
  )
;functie ajutatoare pentru new_world
(define modify_spheres-
  (lambda (world color)
    (if (equal? (car (car world)) 'Spheres)
        (if (and (equal? (car (cdr (car world))) color) (equal? (car (cdr (cdr (car world)))) (my_loc world)))
            (cons (list 'Spheres color (my_loc world) (- (car (cdr (cdr (cdr (car world))))) 1)) (cdr world))
            (cons (car world) (modify_spheres- (cdr world) color))
            )
        (cons (car world) (modify_spheres- (cdr world) color))
        )
    )
  )
;functie ajutatoare pentru new_world
(define modify_carries+
  (lambda (world color)
    (if (equal? (car (car world)) 'Carries)
        (if (equal? (car (cdr (car world))) color) 
            (cons (list 'Carries color (+ (car (cdr (cdr (car world)))) 1)) (cdr world))
            (cons (car world) (modify_carries+ (cdr world) color))
            )
        (cons (car world) (modify_carries+ (cdr world) color))
        )
    )
  )
;functie ajutatoare pentru new_world
(define modify_spheres+
  (lambda (world color)
    (if (equal? (car (car world)) 'Spheres)
        (if (and (equal? (car (cdr (car world))) color) (equal? (car (cdr (cdr (car world)))) (my_loc world)))
            (cons (list 'Spheres color (my_loc world) (+ (car (cdr (cdr (cdr (car world))))) 1)) (cdr world))
            (cons (car world) (modify_spheres+ (cdr world) color))
            )
        (cons (car world) (modify_spheres+ (cdr world) color))
        )
    )
  )
;functie ajutatoare pentru new_world
(define modify_carries-
  (lambda (world color)
    (if (equal? (car (car world)) 'Carries)
        (if (equal? (car (cdr (car world))) color) 
            (cons (list 'Carries color (- (car (cdr (cdr (car world)))) 1)) (cdr world))
            (cons (car world) (modify_carries- (cdr world) color))
            )
        (cons (car world) (modify_carries- (cdr world) color))
        )
    )
  )
;functie ajutatoare pentru new_world
(define modify_location
  (lambda (world new_loc)
     (if (equal? (car (car world)) 'Location)
         (cons (list 'Location new_loc) (cdr world))
         (cons (car world) (modify_location (cdr world) new_loc))
         )
    )
  )
;functie care intoarce toti vecinii unui nod din graf
(define get_children
  (lambda (child world)
    (if (null? world)
        '()
        (if (equal? (car (car world)) 'Door)
            (if (equal? (car (cdr (car world))) child)
                (cons (car (cdr (cdr (car world)))) (get_children child (cdr world)))
                (get_children child (cdr world))
                )
            (get_children child (cdr world))
            )
        )
    )
  )
;functie care implementeaza dfs 
(define path_home
  (lambda (children end explored world)    
    (let*
        (
         (_children (minus children explored))
         
         )
      
        (if (null? _children )
            '()  
            (let*
                (
                 (first (car _children))
                 )
              (if (equal? first end)
                  (list end)
                  (if (null? (path_home (get_children first world) end  (cons first explored) world))
                      (if (null? (cdr _children))
                          '()
                          (path_home (cdr _children) end (cons first explored) world)
                          )
                      
                      (cons first (path_home (get_children first world) end (cons first explored) world))
                      
                      )
                  )
              )
            )
      )
    )
  )
;functie ajutatoare pentru bfs
(define bfs-aux 
  (lambda (children end explored world)
    (if (null? (minus children explored))
        '()
        (let*
            (
             (list1 (bfs (car (minus children explored )) end explored world))
             (list2 (bfs-aux (cdr (minus children explored)) end explored world))
             )
          (if (null? list1)
                list2
              (if (null? list2)
                    list1
                  (if (<= (length list1) (length list2))
                        list1
                        list2
                      )
                  )
              )
          )
        )
    )
  )
;functie care implementeaza bfs si gaseste cea mai scurta cale catre un nod destinatie  
(define bfs
  (lambda (start end explored world)
    (if (equal? start end)
        (list end)
          (let*
              (
               (lista (bfs-aux (get_children start world) end (cons start explored) world))
               )
            (if (null? lista)
                '()
                (cons start lista)
                )
            )
          )
    )  
  )
;functie auxiliara pentru make-moves
(define make_moves-aux
  (lambda (lista)
    (if (null? (cdr lista))
        '()
        (cons (list 'Move (car lista) (car (cdr lista))) (make_moves-aux (cdr lista)))
        )
    )
  )
;functie care creaza o lista de tip ( (Move X Y) (Move Y Z)) pentru x inceput si y end folosind dfs
(define make_moves
  (lambda (start stop world)
    (if (null? (path_home (list start) stop '() world))
        '()
        (make_moves-aux (bfs start stop '() world))
        )
    )
  )
;functie care creaza o lista de tip ( (Move X Y) (Move Y Z)) pentru x inceput si y end folosind bfs
(define make_moves_bfs
  (lambda (start stop world)
    (if (null? (bfs start stop '() world))
        '()
        (make_moves-aux (bfs start stop '() world))
        )
    )
  )
 ;functie care face update pe lume dupa aplicarea unei secvente de operatii de Move         
(define new_world_after_moves
 (lambda (world list_moves)
   (if (null? list_moves)
       world
       (new_world_after_moves (new_world world (car list_moves)) (cdr list_moves))
       )
   )
 )
;functie care face update la lume dupa aplicarea unei operatii
(define new_world
  (lambda (world actiune)
    (if (equal? (car actiune) 'Load)
        (modify_carries+ (modify_spheres- world (car (cdr actiune))) (car (cdr actiune)))
        (if (equal? (car actiune) 'Move)
            (modify_location world (car (cdr (cdr actiune))))
            (if (equal? (car actiune) 'Unload)
                (modify_carries- (modify_spheres+ world (car (cdr actiune))) (car (cdr actiune)))
                )
            )
        )
    )
  )
 ;agentul memoryless-agent       
(define my_agent
  (lambda (goal world rest info)
    (if (equal? rest 'Final)
        ;stare finala: actiunea anterioara a fost "unload"
          '()
         (begin
              ;Robot in locatie finala?
              (if (final_loc  world goal)
                  ;Da
                  ;Robot cara bila de culoare dorita?
                  (if (carry_the_ball world goal)
                      ;Da
                      ;Ne aflam in starea finala a robotului
                      ;Adauga "Unload " la lista de actiuni si trimite mai departe Final cu sensul ca la urmatoarea recursivitate sa trimita lista finala
                      (cons (list 'Unload (the_color goal)) (my_agent goal world 'Final (cons (my_loc world) info)))
                      ;Nu
                      ;Robot cara altceva si robot nu e full?
                      (let* 
                          (
                           (locatie (find_loc_has_the_ball world goal))
                           (color (the_color goal))
                           (lista_moves (if (not (equal? locatie #f))
                                            (make_moves (my_loc world) locatie world)
                                            (make_moves (my_loc world) (the_loc goal) world)
                                            )
                                        )
                           (move_list_minus (if (null? (minus (get_neighbours world) info))
                                                (list 'Move (my_loc world) (car (minus-el (my_loc world) (get_neighbours world))))
                                                (list 'Move (my_loc world) (car (minus (get_neighbours world) info)))
                                                )
                                            )
                           )
                        (if (and (carry_sth_else world goal) (not (robot_full world)))
                            ;Da
                            ;Exista sfere de alta culoare in loc curenta?  
                            (if (not (equal? (loc_has_more_balls_any world goal) #f))
                                ;Da
                                ;Adauga "Load Grey/Blue/Red" la lista de actiuni
                                ;in info tin lista de noduri deja vizitate. Adaug la info nodul curent(locatia curenta)
                                ;Scad din mutimea de vecini pe cei deja vizitati
                                ;daca lista ramane goala aleg unul oricare..primul
                                ; (if (null? (minus (get_neighbours world) (cons (my_loc world) info))) 
                                (cons (list 'Load (loc_has_more_balls_any world goal)) (append lista_moves  (my_agent goal (new_world_after_moves (new_world world (list 'Load (loc_has_more_balls_any world goal))) lista_moves) rest (cons (my_loc world) info))))
                                ;Nu
                                ;Adauga unload sfera la plan
                                (cons (list 'Unload (get_carried_ball world)) (append lista_moves (my_agent goal (new_world_after_moves  (new_world world (list 'Unload (get_carried_ball world))) lista_moves) rest (cons (my_loc world) info))))
                                )
                            ;Nu
                            (if (null? lista_moves)
                                (my_agent goal world 'Final info)
                                (append lista_moves (my_agent goal (new_world_after_moves world lista_moves) rest (cons (my_loc world) info)))
                                )
                            )
                        )
                      
                      )
                  ;Nu
                  ;Robot cara bila de culoare dorita?
                  (let*
                      (
                       (move_list (make_moves (my_loc world) (the_loc goal) world)) 
                       (trio_complet (find_loc_has_2_random_balls world goal))
                       (location_2 (if (equal? trio_complet #f)
                                       (the_loc goal)
                                       (car trio_complet)
                                       )
                                   )
                       (move_list2 
                        (if (and (not (equal? (my_loc world) location_2)) (not (equal? location_2 #f)))
                            (make_moves (my_loc world) location_2 world)
                            (make_moves (my_loc world) (the_loc goal) world)
                            )
                        )
                       (locatie (find_loc_has_the_ball world goal))
                       (color (the_color goal))
                       (lista_moves (if (not (equal? locatie #f))
                                        (make_moves (my_loc world) locatie world)
                                        (make_moves (my_loc world) (the_loc goal) world)
                                        )
                                    ) 
                       )
                    (if (carry_the_ball world goal)
                        ;Da
                        ;Robot poate sa se miste?
                        (if (can_move world)
                            ;Da
                            ;Ia path_home ca sa te intorci acasa 
                            (append  move_list (my_agent goal (new_world_after_moves world move_list) rest info))  
                            ;Nu
                            (cons (list 'Unload (the_color goal)) (my_agent goal (new_world world (list 'Unload (the_color goal))) rest info))
                            )
                        ;Nu
                        ;Robot poate sa se miste?
                        (if (not (can_move world))
                            ;Nu
                            (cons (list 'Unload (get_carried_ball world)) (my_agent goal (new_world world (list 'Unload (get_carried_ball world))) rest info))
                            ;Da
                            ;Exista Bila cautata in locatia curenta?
                            (if (loc_has_the_ball world goal)
                                ;Da
                                ;Robot full?
                                (if (robot_full world)
                                    ;Da
                                    ;Intai aplicam 'Unload pe o bila pe care o cara", dupa incarcam bila dorita "load the ball" si dupa cautam vecinii deja explorati ca sa se intoarca acasa
                                    (let* 
                                        (
                                         (the_new_world (new_world_after_moves (new_world (new_world world (list 'Unload (get_carried_ball world))) (list 'Load (the_color goal))) move_list))
                                         )
                                      (cons (list 'Unload (get_carried_ball world)) (cons (list 'Load (the_color goal)) (append move_list (my_agent goal the_new_world rest info))))
                                      )
                                    ;Nu
                                    ;Exista aici si alta bila ca sa se poata deplasa?
                                    (if (not (equal? (loc_has_more_balls world goal) #f))
                                        ;Da
                                        ;adauga bila "load the ball" si apoi si ceallacta bila  'Load blue/red/gray' si apoi cauta vecin ca sa te intorci acasa
                                        (let*
                                            (
                                             (the_new_world (new_world_after_moves (new_world (new_world world (list 'Load (the_color goal))) (list 'Load (loc_has_more_balls world goal))) move_list))
                                             )
                                          (cons (list 'Load (the_color goal)) (cons (list 'Load (loc_has_more_balls world goal)) (append move_list (my_agent goal the_new_world rest info))))
                                          )
                                        ;Nu
                                        ;Sunt in mod de cautare ; rest = "Cautare"
                                        (append move_list2 (my_agent goal (new_world_after_moves world move_list2) (list "Cautare" (my_loc world)) (cons (my_loc world) info)))
                                        )
                                    )
                                ;Nu
                                ;Ma aflu in mod de cautare? ; rest == "Cautare" ?
                                (if (and (not (null? rest)) (equal? (car rest) "Cautare"))
                                    ;Da
                                    ;Exista oricare doua bile in loc asta?
                                    (if (and (not (equal? (loc_has_more_balls_any world goal) #f)) (not (equal? (loc_has_more_balls_any (new_world world (list 'Load (loc_has_more_balls_any world goal))) goal) #f)))
                                        ;Da
                                        ;load bila1 ; load bila 2; cauta oricare vecini
                                        (let*
                                            (
                                             (tel goal)
                                             (bila1 (loc_has_more_balls_any world tel))
                                             (bila2 (loc_has_more_balls_any (new_world world (list 'Load bila1)) tel))
                                             (the_new_world (new_world_after_moves (new_world (new_world world (list 'Load bila1)) (list 'Load bila2)) lista_moves))
                                             )
                                          
                                          (cons (list 'Load bila1) (cons (list 'Load bila2) (append lista_moves (my_agent goal the_new_world '() (cdr info)))))
                                          )
                                        ;Nu
                                        ;Cauta vecini  
                                        (append move_list2 (my_agent goal (new_world_after_moves world move_list2) '() (cons (my_loc world) info)))
                                        )
                                    ;Nu
                                    ;Cauta vecini 
                                    (append move_list2 (my_agent goal (new_world_after_moves world move_list2) 'Final (cons (my_loc world) info)))
                                    )
                                )
                            )
                        )
                    )
                  )
            )
        )
    )
  )
              
                               
 (define help_agent 
   (lambda (goal world rest info)
     (list (my_agent goal world '() '()))
     )
   )
  
 

(define find_loc_has_the_ball
  (lambda (world goal)
    (if (null? world)
        #f
        (if (and (and (and (equal? (car (car world)) 'Spheres) (equal? (car (cdr (car world))) (the_color goal))) (> (car (cdr (cdr (cdr (car world))))) 0)) (not (equal? (car (cdr (cdr (car world)))) (the_loc goal))))
            (car (cdr (cdr (car world))))
            (find_loc_has_the_ball (cdr world) goal)
            )
        )
    )
  )

(define loc_has_more_balls_any_loc
  (lambda (loc world goal)
    (if (null? world)
        #f
        (if (equal? (car (car world)) 'Spheres) 
            (if (and (equal? (car (cdr (cdr (car world)))) loc) (> (car (cdr (cdr (cdr (car world))))) 0))
                (if (or  (and (equal? loc 'RedWarehouse) (equal? (car (cdr (car world))) 'Red)) (and (equal? loc 'BlueWarehouse) (equal? (car (cdr (car world))) 'Blue)))
                    (loc_has_more_balls_any_loc loc (cdr world) goal)
                    (car (cdr (car world)))
                    )
                (loc_has_more_balls_any_loc loc (cdr world) goal)
                )  
            (loc_has_more_balls_any_loc loc (cdr world) goal)
            )
        )
    )
  )
(define find_loc_has_2_random_balls
  (lambda (world goal)
    (if (null? world)
        #f
       (if (equal? (car (car world)) 'Spheres)
            (let*
                (
                 (color (car (cdr (car world))))
                 (nr_balls (car (cdr (cdr (cdr (car world))))))
                 (location (car (cdr  (cdr (car world)))))
                 (rez (loc_has_more_balls_any_loc location (new_world (modify_location world location) (list 'Load color)) goal))
                 )
              (if (and  (> nr_balls 0) (not (equal? rez #f)) )
                  (list location color rez)
                  (find_loc_has_2_random_balls (cdr world) goal)
                  )
              )
            (find_loc_has_2_random_balls (cdr world) goal)
            )
          )
        )
    )



(define carry_one_ball
  (lambda (world)
    (if (and (not (robot_full world)) (not (robot_empty world)))
        #t
        #f
        )
    )
  )

(define profi_agent
  (lambda (goal world rest info)
    ;Loc finala?
    (if (final_loc world goal)
        ;Da
        ;Cari ceva?
        (if (carry_one_ball world)
            ;Da
            ;Exista alte bile aici fara bila B, Load o alta bila, Move to locatie si testeaza daca exista acolo bila B, Load bila B
            (let*
                (
                 (rez (loc_has_more_balls_any world goal))
                 )
              (if (not (equal? rez #f))
                  ;Da
                  ;Gaseste locatia unde se afla o bila B
                  (if (not (find_loc_has_the_ball world goal))
                      (list (list (list 'Load (the_color goal))))
                      
                      (let*
                          (
                           (locatie (find_loc_has_the_ball world goal))
                           (loc_mea (my_loc world))
                           (bila_carata (carry_sth_else2 world goal))
                           (lista_moves  (begin
                                           (display "\n Locatie = ")
                                           (display locatie)
                                           (display "\n My_loc = ")
                                           (display loc_mea)
                                           (display "\n")
                                           (make_moves_bfs locatie (the_loc goal) world))
                                         )
                           (color (the_color goal))
                           (lista_test (list 'Test (list 'Spheres color locatie 'n) (list 'Positive 'n))) 
                           (lista_totala (append (append (list lista_test (list 'Unload bila_carata) (list 'Load color) ) lista_moves) (list (list 'Unload color))))
                           )
                        
                        
                        (list (cons (list 'Load rez) (append (make_moves_bfs loc_mea locatie world) lista_totala)))
                        
                        )
                      )
                  ;Nu
                  ;Gaseste locatia cu bila B, Vezi daca mai exista si alte bile
                  (let*
                      (
                       (locatie (find_loc_has_the_ball world goal))
                       (world_with_new_loc (modify_location world locatie))
                       (rez (loc_has_more_balls_any world_with_new_loc goal))
                       )
                    (if (not (equal? rez #f))
                        ;Da
                        ;Unload bila veche pe care o cara, Move to new location, Test daca exista acolo bila B si o alta bila oarecare
                        (let*
                            (
                             (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                             (color (the_color goal))
                             (lista_test (if (equal? color rez)
                                             (list 'Test (list 'Spheres (the_color goal) locatie 'n) (list 'Succ 'n1 'n) (list 'Positive 'n1)) 
                                             (list 'Test (list 'Spheres (the_color goal) locatie 'n) (list 'Spheres rez locatie 'n1) (list 'Positive 'n) (list 'Positive 'n1))
                                             )
                                         )
                             (lista_totala (append (append (list lista_test (list 'Load color) (list 'Load rez) ) lista_moves) (list (list 'Unload color))))
                             (loc_mea (my_loc world))
                             )
                          (list (cons (list 'Unload (carry_sth_else2 world goal)) (append (make_moves_bfs loc_mea locatie world) lista_totala)))
                          )
                        ;Nu
                        ;Gaseste locatie cu doua bile oarecare pe care sa le cari ca sa schimbi una dintre ele la un moment dat cu bila rosie
                        (if (equal? (find_loc_has_2_random_balls world goal) #f)
                            '(())
                            
                            (let*
                                (
                                 
                                 (trio_complet (find_loc_has_2_random_balls world goal))
                                 (location_2 (car trio_complet))
                                 (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                                 (lista_moves_f (make_moves_bfs location_2 locatie world))
                                 (color (the_color goal))
                                 (bila1 (car (cdr trio_complet)))
                                 (bila2 (car (cdr (cdr trio_complet))))
                                 (lista_test (if (equal? bila1 bila2)
                                                 (list 'Test (list 'Spheres bila1 location_2 'n) (list 'Succ 'n1 'n) (list 'Positive 'n1))   
                                                 (list 'Test (list 'Spheres bila1 location_2 'n) (list 'Spheres bila2 location_2 'n1) (list 'Positive 'n))
                                                 )
                                             )
                                 (lista_test_aux (list 'Test (list 'Spheres color locatie 'n) (list 'Positive 'n)))
                                 (lista_partiala (append (list lista_test_aux (list 'Unload bila1) (list 'Load color)) lista_moves))                
                                 (lista_totala (append (append (append (list lista_test (list 'Load bila1) (list 'Load bila2)) lista_moves_f) lista_partiala) (list (list 'Unload color))))
                                 (loc_mea (my_loc world))
                                 )
                              (list (cons (list 'Unload (carry_sth_else2 world goal)) (append (make_moves_bfs loc_mea location_2 world) lista_totala)))
                              )
                            )
                        )
                    )
                  )
              )
            ;Nu, nu car nimic
            ;Gaseste locatia cu bila B, Vezi daca mai exista si alte bile
            (let*
                (
                 (locatie (find_loc_has_the_ball world goal))
                 (color (the_color goal))
                 (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                 (world_with_new_loc (modify_location world locatie))
                 (rez (loc_has_more_balls_any world_with_new_loc goal))
                 )
              (if (not (equal? rez #f))
                  ;Da
                  ; Move to new location, Test daca exista acolo bila B si o alta bila oarecare
                  (let*
                      (
                       (color (the_color goal))
                       (lista_test (if (equal? (the_color goal) rez)
                                       (list 'Test (list 'Spheres (the_color goal) locatie 'n) (list 'Succ 'n1 'n) (list 'Positive 'n1)) 
                                       (list 'Test (list 'Spheres (the_color goal) locatie 'n) (list 'Spheres rez locatie 'n1) (list 'Positive 'n) (list 'Positive 'n1))
                                       )
                                   )
                       (lista_totala (append (append (list lista_test (list 'Load color) (list 'Load rez) ) lista_moves) (list (list 'Unload color))))
                       (loc_mea (my_loc world))
                       )
                    (list (append (make_moves_bfs loc_mea locatie world) lista_totala))
                    )
                  ;Nu
                  ;Gaseste locatie cu doua bile oarecare pe care sa le cari ca sa schimbi una dintre ele la un moment dat cu bila rosie
                  (let*
                      (
                       (trio_complet (find_loc_has_2_random_balls world goal))
                       (location_2 (car trio_complet))
                       (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                       (lista_moves_f (make_moves_bfs location_2 locatie world))
                       (color (the_color goal))
                       (bila1 (car (cdr trio_complet)))
                       (bila2 (car (cdr (cdr trio_complet))))
                       (lista_test (if (equal? bila1 bila2)
                                       (list 'Test (list 'Spheres bila1 location_2 'n) (list 'Succ 'n1 'n) (list 'Positive 'n1))   
                                       (list 'Test (list 'Spheres bila1 location_2 'n) (list 'Spheres bila2 location_2 'n1) (list 'Positive 'n))
                                       )
                                   )
                       (lista_test_aux (list 'Test (list 'Spheres color locatie 'n) (list 'Positive 'n)))
                       (lista_partiala (append (list lista_test_aux (list 'Unload bila1) (list 'Load color)) lista_moves))                
                       (lista_totala (append (append (append (list lista_test (list 'Load bila1) (list 'Load bila2)) lista_moves_f) lista_partiala) (list (list 'Unload color))))
                       (loc_mea (my_loc world))
                       )
                    (list (append (make_moves_bfs loc_mea location_2 world) lista_totala))
                    )
                  )
              )
            )
        ;Nu , nu e loc finala
        ;Partea de replanificare
        ;Rezultat test == False sau mutare gresita
          
        ;False
        (let*
            (
             (loc (find_loc_has_the_ball world goal))
             (locatie (if (equal? #f loc)
                          (the_loc world)
                          loc))
             (world_with_new_loc (modify_location world locatie))
             (rez (loc_has_more_balls_any world_with_new_loc goal))
             )
          ;Daca robotul este full
          (if (robot_full world)
              (let*
                  (
                   (color (the_color goal))
                   (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                   (lista_test (list 'Test (list 'Spheres color locatie 'n) (list 'Positive 'n)))         
                   (lista_totala (append (append (list lista_test (list 'Unload (carry_sth_else2 world goal)) (list 'Load color) ) lista_moves) (list (list 'Unload color))))
                   (loc_mea (my_loc world))
                   )
                (list (append (make_moves_bfs loc_mea locatie world) lista_totala))
                )
              (if (not (equal? rez #f))
                  ;Da
                  ;Move to new location, Test daca exista acolo bila B si o alta bila oarecare
                  (let*
                      (
                       (color (the_color goal))
                       (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                       (lista_test (if (equal? (the_color goal) rez)
                                       (list 'Test (list 'Spheres (the_color goal) locatie 'n) (list 'Succ 'n1 'n) (list 'Positive 'n1)) 
                                       (list 'Test (list 'Spheres (the_color goal) locatie 'n) (list 'Spheres rez locatie 'n1) (list 'Positive 'n) (list 'Positive 'n1))
                                       )
                                   )
                       (lista_totala (append (append (list lista_test (list 'Load color) (list 'Load rez) ) lista_moves) (list (list 'Unload color))))
                       (loc_mea (my_loc world))
                       )
                    (if (carry_the_ball world goal)
                        (list (cons (list 'Unload (the_color goal)) (append (make_moves_bfs loc_mea locatie world) lista_totala)))
                        (if (carry_sth_else2 world goal)
                            (list (cons (list 'Unload (carry_sth_else2 world goal)) (append (make_moves_bfs loc_mea locatie world) lista_totala)))
                            (list (append (make_moves_bfs loc_mea locatie world) lista_totala))
                            )
                        )
                    
                    )
                  ;Nu
                  ;Gaseste locatie cu doua bile oarecare pe care sa le cari ca sa schimbi una dintre ele la un moment dat cu bila rosie
                  (if (equal? (find_loc_has_2_random_balls world goal) #f)
                      (display "CEVA")
                      (let*
                          (
                           (trio_complet (find_loc_has_2_random_balls world goal))
                           (location_2 (car trio_complet))
                           (color (the_color goal))
                           (lista_moves (make_moves_bfs locatie (the_loc goal) world))
                           (lista_moves_f (make_moves_bfs location_2 locatie world))
                           (bila1 (car (cdr trio_complet)))
                           (bila2 (car (cdr (cdr trio_complet))))
                           (lista_test (if (equal? bila1 bila2)
                                           (list 'Test (list 'Spheres bila1 location_2 'n) (list 'Succ 'n1 'n) (list 'Positive 'n1))   
                                           (list 'Test (list 'Spheres bila1 location_2 'n) (list 'Spheres bila2 location_2 'n1) (list 'Positive 'n))
                                           )
                                       )
                           (lista_test_aux (list 'Test (list 'Spheres color locatie 'n) (list 'Positive 'n)))
                           (lista_partiala (append (list lista_test_aux (list 'Unload bila1) (list 'Load color)) lista_moves))                
                           (lista_totala (append (append (append (list lista_test (list 'Load bila1) (list 'Load bila2)) lista_moves_f) lista_partiala) (list (list 'Unload color))))
                           (loc_mea (my_loc world))
                           )
                        (if (carry_the_ball world goal)
                            (list (cons (list 'Unload (the_color goal)) (append (make_moves_bfs loc_mea location_2 world) lista_totala)))
                            (if (carry_sth_else2 world goal)
                                (list (cons (list 'Unload (carry_sth_else2 world goal)) (append (make_moves_bfs loc_mea locatie world) lista_totala)))
                                (list (append (make_moves_bfs loc_mea location_2 world) lista_totala))
                                )
                            )
                        )
                      )
                  )
              )
          )
        )
    )
  )

(define memoryless-agent help_agent)
(define advanced-agent profi_agent)                            
                          
                          
                          
                      
                  
                                                                                                                         
                                                                                                                         
                        
                    
                
                
                





                 
