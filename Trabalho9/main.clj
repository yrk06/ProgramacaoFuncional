; 1. Utilizando  a  linguagem  Clojure,  crie  uma  função  chamada  ultimo  que  receba  uma  lista  e devolva o último elemento desta lista sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn ultimo [lista] (
  nth lista (dec (count lista))                
))

(println (format "Func 1. Entrada: %s. Saida: %d." (str [1 2 3 4 5]) (ultimo [1 2 3 4 5]) ))


(defn penultimo [lista] (
  nth lista (dec (dec (count lista)))      
))

; 2. Utilizando a linguagem Clojure, crie uma função chamada penultimo que receba uma lista e  devolva  o  penúltimo  elemento  desta  lista  usar as  funções  já  prontas  e disponíveis para esta mesma finalidade na linguagem Clojure.  
(println ( format "Func 2. Entrada: %s. Saida: %d." (str [1 2 3 4 5]) (penultimo [1 2 3 4 5]) ))

; 3. Utilizando a linguagem Clojure, crie uma função chamada elementoN que receba uma lista e um inteiro N e devolva o  elemento que  está na  posição N desta lista usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn elementoN
        [lista n]
        (loop [contador n resto-lista lista]
              (if (zero? contador)
                  (first resto-lista)
                (recur (dec contador) (rest resto-lista))
              )
        )
)

(println (format "Func 3. Entrada: %s %d. Saida %d." (str [1 2 3 4 5]) 3 (elementoN [1 2 3 4 5] 3) ))

; 4. Utilizando  a  linguagem Clojure,  crie  uma função  chamada  inverso  que  receba uma  lista  e devolva esta lista com as posições dos elementos invertidas. Por exemplo recebe [1,2,3] e devolve [3,2,1]. Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.

(defn inverso [coll]
  (loop [coll coll
         acc  (empty coll)]
        (if (empty? coll)
            acc
            (recur (rest coll) (cons (first coll) acc)))))

(println (format "Func 4. Entrada: %s. Saida: %s" (str [1 2 3 4 5]) (str (inverso [1 2 3 4 5]) )))

; 5. Utilizando a  linguagem Clojure, crie uma função chamada  mdc que receba  dois inteiros e devolve o mínimo divisor comum entre eles.  Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn mdc [a b]
  (loop [candidato (max a b) ]
      (if 
        (or 
          (identical? candidato 1) 
          (and 
            (zero?
              (mod a candidato)
            ) 
            (zero?
              (mod b candidato)
            ) 
          ) 
        )
        candidato
        (recur (dec candidato))
      )  
  )  
)

(println (format "Func 5. Entrada: %d %d. Saida: %d" 12 20 (mdc 12 20)))