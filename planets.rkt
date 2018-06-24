;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab5(NossoUltimoLab)|) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp")) #t)))
;;Definição de dados:

(define-struct asteroide (raio posx posy dx dy cor))
;;Um elemento asteroide de Asteroide é uma estrutura
;;(make-asteroide (um-r um-x um-y um-dx um-dy uma-cor)), onde:
;;um-r: Número, é o raio do asteroide
;;um-x: Número, é a posição da bola no eixo x
;;um-y: Número, é a posição da bola no eixo y
;;um-dx: Número, é o deslocamento da bola no eixo x
;;um-dy: Número, é o deslocamento da bola no eixo y
;;uma-cor: Símbolo, é a cor do asteroide

(define-struct planeta (posx posy mat raio raio-g cor))
;;Um elemento planeta de Planeta é uma estrutura
;;(make-planeta (um-x um-y um-mat um-r um-rg uma-cor)), onde:
;;um-x: Número, é a posição do planeta no eixo x
;;um-y: Número, é a posição do planeta no eixo y
;;um-mat: Símbolo, é o material do qual é feito o planeta
;;um-r: Número, é o raio do planeta
;;um-rg: Número, é o raio do campo gravitacional do planeta
;;uma-cor: Símbolo, é a cor do planeta

(define A1 (make-asteroide 10 150 20  18 5  'red))
(define A2 (make-asteroide 15 720 550 -8 -5 'red)) 

(define P1 (make-planeta 200 380 'ferro 40 180 'black))
(define P2 (make-planeta 900 120 'gelo  50 150 'blue))

(define Planetas (list P1 P2))

(define Asteroides (list A1 A2))

;;Uma Lista-de-planetas é:
;;1. ou empty
;;2. ou (cons p ldp), onde:
;;-p: Planeta
;;-ldp: Lista-de-planetas

(define LARG   1000)
(define ALT    1000)
(define ESPERA 0.1)

;;movimenta-asteroide: Asteroide -> Asteroide
;;Objetivo: Recebe um asteroide e cria outro, incrementando seu deslocamento à sua posição
;;Exemplos: (movimenta-asteroide A1) -> (make-asteroide 10 168 25  18 5  'red)
;;          (movimenta-asteroide A2) -> (make-asteroide 15 712 545 -8 -5 'red)
(define (movimenta-asteroide a)
  (make-asteroide (asteroide-raio a)
                  (+ (asteroide-posx a)
                     (asteroide-dx   a))
                  (+ (asteroide-posy a)
                     (asteroide-dy   a))
                  (asteroide-dx  a)
                  (asteroide-dy  a)
                  (asteroide-cor a))
  )


(check-expect (movimenta-asteroide A1) (make-asteroide 10 168 25  18 5  'red))
(check-expect (movimenta-asteroide A2) (make-asteroide 15 712 545 -8 -5 'red))


;;verifica-algum-campo-gravitacional: Asteroide Lista-de-planetas -> Booleano
;;Objetivo: Recebe um asteroide e uma lista de planetas e verifica se o asteroide recebido está dentro do campo gravitacional de algum dos planetas da lista dada
;;Exemplos: (verifica-algum-campo-gravitacional A1 empty)    -> false
;;          (verifica-algum-campo-gravitacional A2 empty)    -> false
;;          (verifica-algum-campo-gravitacional A1 Planetas) -> false
;;          (verifica-algum-campo-gravitacional A2 Planetas) -> false
;;          (verifica-algum-campo-gravitacional (make-asteroide 2 900 220 1 1 'blue) Planetas) -> true
(define (verifica-algum-campo-gravitacional a ldp)
  (cond
    [(empty? ldp) false]
    [else
     (or
      (cond
        [(>= (planeta-raio-g (first ldp))
             (sqrt (+ (sqr (- (asteroide-posx a) (planeta-posx (first ldp))))(sqr (- (asteroide-posy a) (planeta-posy (first ldp)))))))
         true]
        [else false])
      (verifica-algum-campo-gravitacional a (rest ldp)))]
    )
  )


(check-expect (verifica-algum-campo-gravitacional A1 empty) false)
(check-expect (verifica-algum-campo-gravitacional A2 empty) false)
(check-expect (verifica-algum-campo-gravitacional A1 Planetas) false)
(check-expect (verifica-algum-campo-gravitacional A2 Planetas) false)
(check-expect (verifica-algum-campo-gravitacional (make-asteroide 2 900 220 1 1 'blue) Planetas) true)


;;verifica-campo-gravitacional: Asteroide Planeta -> Booleano
;;Objetivo: Recebe um asteroide e um planeta e verifica se o planeta recebido está no campo gravitacional daquele planeta
;;Exemplos: (verifica-campo-gravitacional A1 P1) -> false
;;          (verifica-campo-gravitacional A1 P2) -> false
;;          (verifica-campo-gravitacional A2 P1) -> false
;;          (verifica-campo-gravitacional A2 P2) -> false
(define (verifica-campo-gravitacional a p)
  (cond
    [(>= (planeta-raio-g p)
         (sqrt (+ (sqr (- (asteroide-posx a) (planeta-posx p)))(sqr (- (asteroide-posy a) (planeta-posy p))))))
     true]
    [else false]
    )
  )


(check-expect (verifica-campo-gravitacional A1 P1) false)
(check-expect (verifica-campo-gravitacional A1 P2) false)
(check-expect (verifica-campo-gravitacional A2 P1) false)
(check-expect (verifica-campo-gravitacional A2 P2) false)


;;altera-trajetoria-por-planeta: Asteroide Planeta -> Asteroide
;;Objetivo: Recebe um asteróide e um planeta, se o asteróide estiver acima do planeta: diminui 2 do eixo y
;;                                                        se estiver abaixo do planeta: aumenta 2 do eixo y
;;                                                        se estiver a direita do planeta: dimui 2 do eixo x
;;                                                        se estiver a esquerda do planeta: aumeta 2 do eixo x
;; e então devolve o asteróide com suas posições atualizadas.
;;Exemplos:
(define (altera-trajetoria-por-planeta a p)
  (cond
    [(symbol=? (planeta-mat p) 'gelo)
     (cond 
      [(> (asteroide-posy a) (planeta-posy p))
       (make-asteroide (asteroide-raio a)
                       (asteroide-posx a)
                       (+ (asteroide-posy a) (asteroide-dy   a))
                       (asteroide-dx   a)
                       (+ (asteroide-dy   a) 2)
                       (asteroide-cor  a))]
      [(< (asteroide-posy a) (planeta-posy p))
       (make-asteroide (asteroide-raio a)
                       (asteroide-posx a)
                       (+ (asteroide-posy a) (asteroide-dy   a))
                       (asteroide-dx   a)
                       (- (asteroide-dy   a) 2)
                       (asteroide-cor  a))]
      [(> (asteroide-posx a) (planeta-posx p))
       (make-asteroide (asteroide-raio a)
                       (+ (asteroide-posx a) (asteroide-dx   a))
                       (asteroide-posy a)
                       (+ (asteroide-dx   a) 2) 
                       (asteroide-dy   a)
                       (asteroide-cor  a))]
      [(< (asteroide-posx a) (planeta-posx p))
       (make-asteroide (asteroide-raio a)
                       (+ (asteroide-posx a) (asteroide-dx   a))
                       (asteroide-posy a)
                       (- (asteroide-dx   a) 2)
                       (asteroide-dy   a)
                       (asteroide-cor  a))]
      [(and (= (asteroide-posy a) (planeta-posy p)) (= (asteroide-posx a) (planeta-posx p)))
       (make-asteroide (asteroide-raio a)
                       (asteroide-posx a)
                       (asteroide-posy a)
                       (asteroide-dx   a)
                       (asteroide-dy   a)
                       (asteroide-cor  a))])]
   
    [else
     (cond
      [(> (asteroide-posy a) (planeta-posy p))
       (make-asteroide (asteroide-raio a)
                       (asteroide-posx a)
                       (+ (asteroide-posy a) (asteroide-dy   a))
                       (asteroide-dx   a)
                       (- (asteroide-dy   a) 2)
                       (asteroide-cor  a))]
      [(< (asteroide-posy a) (planeta-posy p))
       (make-asteroide (asteroide-raio a)
                       (asteroide-posx a)
                       (+ (asteroide-posy a) (asteroide-dy   a))
                       (asteroide-dx   a)
                       (+ (asteroide-dy   a) 2)
                       (asteroide-cor  a))]
      [(> (asteroide-posx a) (planeta-posx p))
       (make-asteroide (asteroide-raio a)
                       (+ (asteroide-posx a) (asteroide-dx   a))
                       (asteroide-posy a)
                       (- (asteroide-dx   a) 2)
                       (asteroide-dy   a)
                       (asteroide-cor  a))]
      [(< (asteroide-posx a) (planeta-posx p))
       (make-asteroide (asteroide-raio a)
                       (+ (asteroide-posx a) (asteroide-dx   a))
                       (asteroide-posy a)
                       (+ (asteroide-dx   a) 2)
                       (asteroide-dy   a)
                       (asteroide-cor  a))]
      [(and (= (asteroide-posy a) (planeta-posy p)) (= (asteroide-posx a) (planeta-posx p)))
       (make-asteroide (asteroide-raio a)
                       (asteroide-posx a)
                       (asteroide-posy a)
                       (asteroide-dx   a)
                       (asteroide-dy   a)
                       (asteroide-cor  a))]
      )]))

(check-expect (altera-trajetoria-por-planeta A1 P1) (make-asteroide 10 150 25  18 7   'red))
(check-expect (altera-trajetoria-por-planeta A1 P2) (make-asteroide 10 150 25  18 3   'red))
(check-expect (altera-trajetoria-por-planeta A2 P1) (make-asteroide 15 720 545 -8 -7  'red))
(check-expect (altera-trajetoria-por-planeta A2 P2) (make-asteroide 15 720 545 -8 -3  'red))


;;altera-trajetoria: Asteroide Lista-de-planetas -> Asteroide
;;Objetivo: Recebe um asteroide e uma lista de planetas e altera o deslocamento do asteroide recebido de acordo com as posições de todos os planetas da lista dada
;;Exemplos: (altera-trajetoria A1 empty)    -> A1
;;          (altera-trajetoria A2 empty)    -> A2
;;          (altera-trajetoria A1 Planetas) -> A1
;;          (altera-trajetoria A2 Planetas) -> A2
(define (altera-trajetoria a ldp)
  (cond
    [(empty? ldp) a]
    [(verifica-campo-gravitacional a (first ldp))
     (altera-trajetoria (altera-trajetoria-por-planeta a (first ldp)) (rest ldp))]
    [else
     (altera-trajetoria a (rest ldp))]
    )
  )


(check-expect (altera-trajetoria A1 empty) A1)
(check-expect (altera-trajetoria A2 empty) A2)
(check-expect (altera-trajetoria A1 Planetas) A1)
(check-expect (altera-trajetoria A2 Planetas) A2)

 
;;verifica-colisao: Asteroide Lista-de-planetas -> Booleano
;;Objetivo: Recebe um asteroide e uma lista de planetas e verifica se o asteroide recebido colidiu com algum dos planetas da lista dada
;;Exemplos: (verifica-colisao A1 empty)    -> false
;;          (verifica-colisao A2 empty)    -> false
;;          (verifica-colisao A1 Planetas) -> false
;;          (verifica-colisao A2 Planetas) -> false
(define (verifica-colisao a ldp)
  (cond
    [(empty? ldp) false]
    [else
     (or
      (cond
        [(>= (planeta-raio (first ldp))
             (sqrt (+ (sqr (- (asteroide-posx a)(planeta-posx (first ldp)))) (sqr (- (asteroide-posy a)(planeta-posy (first ldp)))))))
         true]
        [else false])
      (verifica-colisao a (rest ldp)))]
    )
  )


(check-expect (verifica-colisao A1 empty) false)
(check-expect (verifica-colisao A2 empty) false)
(check-expect (verifica-colisao A1 Planetas) false)
(check-expect (verifica-colisao A2 Planetas) false)


;;movimenta-lista-de-asteroides: Lista-de-asteroides Lista-de-planetas -> Lista-de-asteroides
;;Objetivo: Recebe uma lista de planetas e uma lista de asteróides checando se algum asteróide entrou no campo gravitacional de algum planeta, chama a função que atualiza a posição dos asteróides que estiverem
;;dentro do campo gravitacional e devolve uma lista de asteróides com a movimentação atualizada.
;;Exemplos: (movimenta-lista-de-asteroides Asteroides Planetas) -> (cons (make-asteroide 10 168 25 18 5 'red)
;;                                                                 (cons (make-asteroide 15 712 545 -8 -5 'red) empty))
;;          (movimenta-lista-de-asteroides Asteroides empty)    -> (cons (make-asteroide 10 168 25 18 5 'red)
;;                                                                 (cons (make-asteroide 15 712 545 -8 -5 'red) empty))
;;          (movimenta-lista-de-asteroides empty empty)         -> empty
;;          (movimenta-lista-de-asteroides empty Planetas)      -> empty
(define (movimenta-lista-de-asteroides lda ldp) 
  (cond
    [(empty? lda) empty]
    [(verifica-colisao (first lda) ldp)
     (movimenta-lista-de-asteroides (rest lda) ldp)]
    [else
     (cons (movimenta-asteroide (altera-trajetoria (first lda) ldp))
           (movimenta-lista-de-asteroides (rest lda) ldp))]
  )
)

(check-expect (movimenta-lista-de-asteroides empty empty) empty)
(check-expect (movimenta-lista-de-asteroides empty Planetas) empty)
(check-expect (movimenta-lista-de-asteroides Asteroides empty) (cons (make-asteroide 10 168 25 18 5 'red)
                                                               (cons (make-asteroide 15 712 545 -8 -5 'red) empty)))
(check-expect (movimenta-lista-de-asteroides Asteroides Planetas) (cons (make-asteroide 10 168 25 18 5 'red)
                                                                  (cons (make-asteroide 15 712 545 -8 -5 'red) empty)))

;;desenha-asteroides: Lista-de-asteroides -> Booleano
;;Objetivo: Recebe uma lista de asteroides e desenha todos os seus elementos (discos sólidos) na tela
;;Exemplos: (desenha-asteroides empty)      -> true
;;          (desenha-asteroides Asteroides) -> true
(define (desenha-asteroides lda)
  (cond
    [(empty? lda) true]
    [else
     (and (draw-solid-disk (make-posn (asteroide-posx (first lda))
                                      (asteroide-posy (first lda)))
                           (asteroide-raio (first lda))
                           (asteroide-cor  (first lda)))
          (desenha-asteroides (rest lda)))]
    )
  )


(check-expect (desenha-asteroides empty)      true)
(check-expect (desenha-asteroides Asteroides) true)


;;apaga-asteroides: Lista-de-asteroides -> Booleano
;;Objetivo: Recebe uma lista de asteroides (discos sólidos) e os apaga da tela
;;Exemplos: (apaga-asteroides empty)      -> true
;;          (apaga-asteroides Asteroides) -> true
(define (apaga-asteroides lda)
  (cond
    [(empty? lda) true]
    [else
     (and (clear-solid-disk (make-posn (asteroide-posx (first lda))
                                       (asteroide-posy (first lda)))
                            (asteroide-raio (first lda))
                            (asteroide-cor  (first lda)))
          (apaga-asteroides (rest lda)))]
    )
  )


(check-expect (apaga-asteroides empty)      true)
(check-expect (apaga-asteroides Asteroides) true)


;;desenha-apaga-asteroides: Lista-de-asteroides -> Booleano
;;Objetivo: Recebe uma lista de asteroides, desenha seus elementos (discos sólidos) na tela, espera por um período ESPERA, e depois os apaga
;;Exemplos: (desenha-apaga-asteroides empty)      -> true
;;          (desenha-apaga-asteroides Asteroides) -> true
(define (desenha-apaga-asteroides lda)
  (cond
    [(empty? lda) true]
    [else
     (and
      (desenha-asteroides lda)
      (sleep-for-a-while ESPERA)
      (apaga-asteroides lda))]
    )
  )


(check-expect (desenha-apaga-asteroides empty)      true)
(check-expect (desenha-apaga-asteroides Asteroides) true)


;;desenha-lista-de-planetas: Lista-de-planetas -> Booleano
;;Objetivo: Recebe uma lista de planetas e desenha todos os seus elementos na tela
;;Exemplos: (desenha-lista-de-planetas empty)    -> true
;;          (desenha-lista-de-planetas Planetas) -> true
(define (desenha-lista-de-planetas ldp)
  (cond
    [(empty? ldp) true]
    [else
     (and
      (draw-solid-disk (make-posn (planeta-posx (first ldp))
                                 (planeta-posy (first ldp)))
                       (planeta-raio (first ldp))
                       (planeta-cor  (first ldp)))
      (desenha-lista-de-planetas (rest ldp)))]
    )
  )


(check-expect (desenha-lista-de-planetas empty)    true)
(check-expect (desenha-lista-de-planetas Planetas) true)


;;fora-universo?: Lista-de-asteroides -> Booleano
;;Objetivo: Recebe uma lista de asteroides e verifica se estão todos fora dos limites da tela
;;Exemplos: (fora-universo? empty)      -> true
;;          (fora-universo? Asteroides) -> false
(define (fora-universo? lda)
  (cond
    [(empty? lda) true]
    [else
     (and
      (cond
        [(or
          (> (asteroide-posx (first lda)) LARG)
          (> (asteroide-posy (first lda)) ALT))
         true]
        [else false])
      (fora-universo? (rest lda)))]
    )
  )


(check-expect (fora-universo? empty)      true)
(check-expect (fora-universo? Asteroides) false)


;;movimenta-universo: Lista-de-asteroides Lista-de-planetas -> Booleano
;;Objetivo: Recebe uma lista de asteroides e uma lista de planetas e movimenta os asteroides até que todos tenham ou
;;colidido com algum planeta da lista recebida, ou
;;saído dos limites da tela
;;Exemplos: (movimenta-universo empty      empty)    -> true
;;          (movimenta-universo empty      Planetas) -> true
;;          (movimenta-universo Asteroides empty)    -> true
;;          (movimenta-universo Asteroides Planetas) -> true
(define (movimenta-universo lda ldp)
  (cond
    [(or
      (empty? lda)
      (fora-universo? lda))
     true]
    [else
     (and
      (desenha-lista-de-planetas ldp)
      (desenha-apaga-asteroides lda)
      (movimenta-universo (movimenta-lista-de-asteroides lda ldp) ldp))]
    )
  )


(check-expect (movimenta-universo empty      empty)    true)
(check-expect (movimenta-universo empty      Planetas) true)
(check-expect (movimenta-universo Asteroides empty)    true)
(check-expect (movimenta-universo Asteroides Planetas) true)


(start LARG ALT)
(movimenta-universo Asteroides Planetas)
(stop)