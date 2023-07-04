;; Nome: Marcos Luiz Kurth Reckers 00315653

;; ====================
;; TIPO ListaDeString :
;; ====================
;; Uma ListaDeString é
;; 1. empty, ou
;; 2. (cons s lds), onde
;; s : String
;; lds : ListaDeString

;; ===========================
;; TIPO ListaDeStringOUFalse :
;; ===========================
;; Uma ListaDeStringOUFalse é
;; 1. false, ou
;; 2. ListaDeString

;; ===========
;; TIPO MAPA :
;; ===========
;; Um Mapa é
;; 1. empty, ou
;; 2. (cons n g), onde
;; n : Cidade
;; g : Mapa

;; =============
;; TIPO CIDADE :
;; =============
(define-struct cidade (nome vizinhas))
;; Um elemento do conjunto Cidade é um par
;; (make-cidade n v), onde
;; n : String, representa o nome da cidade
;; v : ListaDeString, representa os (nomes das) cidades vizinhas


;; ===========================================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ===========================================================================
(define MAPA (list
               (make-cidade "SantaFe" (list "Denver" "OklahomaCity"))
               (make-cidade "OklahomaCity" (list "SantaFe" "Denver" "KansasCity" "LittleRock"))
               (make-cidade "LittleRock" (list "OklahomaCity" "SaintLouis" "Nashville"))
               (make-cidade "Atlanta" (list "Nashville"))
               (make-cidade "Denver" (list "SantaFe" "OklahomaCity" "KansasCity" "Omaha" "Helena"))
               (make-cidade "KansasCity" (list "Denver" "Omaha" "SaintLouis" "OklahomaCity"))
               (make-cidade "SaintLouis" (list "KansasCity" "Chicago" "LittleRock" "Nashville" "Pittsburgh"))
               (make-cidade "Nashville" (list "SaintLouis" "LittleRock" "Atlanta" "Pittsburgh"))
               (make-cidade "Omaha" (list "Denver" "Duluth" "Chicago" "KansasCity" "Helena"))
               (make-cidade "Chicago" (list "Omaha" "SaintLouis" "Pittsburgh" "Duluth" "Toronto"))
               (make-cidade "Pittsburgh" (list "Nashville" "SaintLouis" "Chicago" "Toronto"))
               (make-cidade "Helena" (list "Winnipeg" "Duluth" "Omaha" "Denver"))
               (make-cidade "Duluth" (list "Helena" "Winnipeg" "SaultStMarie" "Omaha" "Chicago" "Toronto"))
               (make-cidade "Toronto" (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth"))
               (make-cidade "Winnipeg" (list "Helena" "Duluth" "SaultStMarie"))
               (make-cidade "SaultStMarie" (list "Winnipeg" "Duluth" "Toronto"))
              )
  )
;; ===========================================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ===========================================================================
;; está-na-lista? : String  ListaDeString -> Booleano
;; Objetivo : Dada os um nome (string) e uma lista de strings , nesta ordem, 
;;            diz se o nome dado está na lista.
;; Exemplos :
;; (está-na-lista? "SantaFe" empty) = false
;; (está-na-lista? "SantaFe" (list "SantaFe" "OklahomaCity" "KansasCity" "Omaha" "Helena")) = true
;; (está-na-lista? "Pittsburgh" (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth")) = true
;; (está-na-lista? "Pittsburgh" (list "Winnipeg" "Duluth" "Toronto")) = false

;; Corpo da função :
(define (está-na-lista? nome listanomes)
  (cond
      ;; Se a lista for vazia, retorna false.
    [(empty? listanomes) false] 

      ;; Se o nome do primeiro elemento da lista for igual ao nome dado, retorna true.
    [(string=? nome (first listanomes)) true] 

      ;; Senão, realiza a busca no resto da lista.
    [else (está-na-lista? nome (rest listanomes))] 
   )
 )
;; Testes :
 (check-expect(está-na-lista? "SantaFe" empty) false)
 (check-expect(está-na-lista? "SantaFe" (list "SantaFe" "OklahomaCity" "KansasCity" "Omaha" "Helena"))  true)
 (check-expect(está-na-lista? "Pittsburgh" (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth"))  true)
 (check-expect(está-na-lista? "Pittsburgh" (list "Winnipeg" "Duluth" "Toronto"))  false)

;;----------------------------------------------------------------
;; subtrai-lista : ListaDeString ListaDeString -> ListaDeString
;; Objetivo : dadas duas listas de strings, devolve todos os elementos da primeira
;;            lista que não estão na segunda (ou seja, subtrai a segunda lista da primeira).
;; Exemplos :
;; (subtrai-lista (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth") (list "Winnipeg" "Duluth" "Toronto")) = (list "SaultStMarie" "Pittsburgh" "Chicago")
;; (subtrai-lista empty (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth")) = empty
;; (subtrai-lista (list "Denver" "Omaha" "SaintLouis" "OklahomaCity") (list "Denver" "OklahomaCity")) = (list "Omaha" "SaintLouis")
;; (subtrai-lista (list "Denver" "OklahomaCity") (list "Denver" "Omaha" "SaintLouis" "OklahomaCity")) =  empty

;; Corpo da função :
(define (subtrai-lista lista_1 lista_2)
  (cond
     ;; Se a primeira lista for vazia, retorna uma lista vazia.
    [(empty? lista_1) empty]

     ;; Se o promeiro elemento da lista_1 não estiver na lista_2, retorna uma lista com o primeiro elemento e chama a função para o resto da lista_1.
    [(not (está-na-lista? (first lista_1) lista_2)) (cons (first lista_1) (subtrai-lista (rest lista_1) lista_2))]

     ;; Senão, chama a função com o resto da Lista_1.
    [else (subtrai-lista (rest lista_1) lista_2)]
   )
 ) 
;; Testes :
(check-expect(subtrai-lista (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth") (list "Winnipeg" "Duluth" "Toronto")) (list "SaultStMarie" "Pittsburgh" "Chicago"))
(check-expect(subtrai-lista empty (list "SaultStMarie" "Pittsburgh" "Chicago" "Duluth")) empty)
(check-expect(subtrai-lista (list "Denver" "Omaha" "SaintLouis" "OklahomaCity") (list "Denver" "OklahomaCity")) (list "Omaha" "SaintLouis"))
(check-expect(subtrai-lista (list "Denver" "OklahomaCity") (list "Denver" "Omaha" "SaintLouis" "OklahomaCity"))  empty)

;; ===========================================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ===========================================================================
;; vizinhos : String Mapa ListaDeString -> ListaDeString
;; Objetivo : Dados o nome de uma cidade, um mapa e uma lista de nomes de cidades (já visitadas), nesta ordem,
;;            devolve a lista dos nomes das cidades vizinhas deste cidade que não constam da lista de (nomes de
;;            cidades de entrada).
;; Exemplos :

;; Corpo da função :
(define (vizinhos nome mapa lista-visitados)
  (cond
    ;; Se o mapa for vazio, devolve uma lista vazia.
    [(empty? mapa) empty]

    ;; Se o nome da primeira cidade no mapa for igual ao nome dado, subtrai dos vizinhos dessa cidade a lista dada na função.
    [(string=? nome (cidade-nome (first mapa))) (subtrai-lista (cidade-vizinhas (first mapa)) lista-visitados)]

    ;; Senão, realiza a função para o resto do mapa.
    [else (vizinhos nome (rest mapa) lista-visitados)]
   )
 )

;; Testes :
 (check-expect(vizinhos "Denver" MAPA  (list "OklahomaCity" "SaintLouis" "Nashville"))(list "SantaFe" "KansasCity" "Omaha" "Helena"))
;; ===========================================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;; ===========================================================================
;; acha-cidade : String Mapa -> Cidade
;; Objetivo : Dados uma string com um nome de cidade e um mapa, retorna a cidade
;;            com esse nome presente nesse mapa ou empty caso não esteja presente.
;; Exemplos : 
;; (acha-cidade "Winnipeg" MAPA) = (make-cidade "Winnipeg" (list "Helena" "Duluth" "SaultStMarie"))
;; (acha-cidade "PortoAlegre" MAPA) = empty
(define (acha-cidade nome mapa)
  (cond
    [(empty? mapa)empty]
    [(string=? nome (cidade-nome (first mapa)))(first mapa)]
    [else (acha-cidade nome (rest mapa))]
   )
 )
(check-expect(acha-cidade "Winnipeg" MAPA) (make-cidade "Winnipeg" (list "Helena" "Duluth" "SaultStMarie")))
(check-expect(acha-cidade "PortoAlegre" MAPA) empty)

;; ----------------------------------------------------------------
;; encontra-caminho: String String Mapa ListaDeString -> ListaDeStringOUFalse
;; Objetivos : Dados os nome das cidades origem e destino, um mapa e uma lista 
;;             de cidades já visitadas, encontra um caminho entre a origem e o destino.
;;             Se não existir caminho, devolve false.
;; Exemplos :
;; (encontra-caminho "OklahomaCity" "OklahomaCity" MAPA (list "OklahomaCity" "SaintLouis" "Nashville")) = (list "OklahomaCity")
;; (encontra-caminho "LittleRock" "SaintLouis" MAPA empty) = (list "Little Rock" "OklahomaCity" "SaintLouis")
;; (encontra-caminho "PortoAlegre" "SaintLouis" MAPA empty) = false

;; Corpo da função :
  (define (encontra-caminho origem destino mapa lista-visitadas)
   (cond
      ;; Se a origem for igual ao destino, retornar a lista com o destino.
     [(string=? origem destino) (list destino)]
      ;; Senão
     [else
       (local
         ((define visitadas-novo ))
         ((define caminho ….) ))
       (cond
        
        )
      ]
    )
   )
;; Testes :

;;----------------------------------------------------------------------------
;; encontra-caminho-vizinhos: ListaDeString String Mapa ListaDeString -> ListaDeStringOUFalse
;; Objetivo : Dados uma lista de cidades origem, um destino, um mapa e uma lista de 
;;            cidades já visitadas, encontra um caminho entre alguma das origens e o destino. 
;;            Se não existir caminho, devolve false.
;; Exemplos :

;; Corpo da função :
(define (encontra-caminho-vizinhos listaOrigens destino mapa lista-visitadas)
  (cond
    ;; Se a lista de origens for vazia, retornar false.
    [(empty? listaOrigens) false]

    [else

      (local
        ((define caminho ….))
       )
      (cond
       )
     ]
   )
 )

;; Testes :
