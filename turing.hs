module Turing where



-- --------------------------------------------------------------------------
-- 1 Definamos la cinta infinita
-- --------------------------------------------------------------------------
   
    type Cinta = [String]  -- cada casilla de la cinta es una cadena

-- --------------------------------------------------------------------------
-- 2 Escribamos una función que inicialice la cinta en la configuración 
--   inicial. Para ello utilizaremos tres cadenas: 
--
--      - El marcador final
--      - El símbolo en blanco
--      - La palabra que queremos reconocer 
--
-- --------------------------------------------------------------------------

    inicializa :: String -> String -> String -> Cinta
    inicializa "" "" "" = []
    inicializa final blanco cadena = cinta
      where 
        cinta = final : cadenap cadena ++ blancos 
          where
            cadenap :: String -> [String]
            cadenap "" = []
            cadenap (x:xs) = [x] : cadenap xs 
      
            blancos :: [String]
            blancos = blanco : blancos
  

-- --------------------------------------------------------------------------
-- 3 Definamos un par de funciones que nos permitan leer y escribir sobre
--   la cinta. Estas acciones requieren que conozcamos la posición de la
--   celda cuyo contenido queramos leer o sobreescribir. 
--
--   Para que nuestro código sea más legible incluiremos un tipo de dato
--   para la posición de la cabeza lectora dentro de la cinta. Esta posición
--   no será otra cosa que un entero.
--
-- --------------------------------------------------------------------------

    type Posicion = Int

-- --------------------------------------------------------------------------   
-- 4 Comencemos por definir la función «escribe» que coloca un símbolo dado 
--   en la celda de la posición que requiramos.
--
-- --------------------------------------------------------------------------

    escribe :: String -> Cinta -> Posicion -> Cinta
    escribe nuevo cinta pos
      | pos < 0 = cinta
      | otherwise = take pos cinta ++ [nuevo] ++ drop (pos + 1) cinta

-- --------------------------------------------------------------------------
-- 5 La función que lee y devuelve un símbolo de la cinta debe devolver el
--   primer símbolo si el número de la posición indicada es menor a cero. 
-- 
--   Con esto en mente, definamos la función «lee». 
-- --------------------------------------------------------------------------

    lee :: Cinta -> Posicion -> String
    lee cinta pos
      | pos < 0 = head cinta
      | otherwise = cinta !! pos

-- --------------------------------------------------------------------------
-- 6 Definamos un tipo de dato para las configuraciones que
--   adopta la máquina durante su ejecución. 
--
-- --------------------------------------------------------------------------
 
    type Configuracion = (Estado, Cinta, Posicion)

-- --------------------------------------------------------------------------
-- 7 Definamos un tipo de dato para las transiciones de la máquina
--   
--   Como en las implementaciones anteriores incluirémos un tipo
--   de dato para los estados de la unidad de control. 
--
--   Incluirémos un tipo de dato para los movimientos: 
--     D - derecha
--     I - izquierda
--     N - sin movimiento
-- --------------------------------------------------------------------------
    data Mov = D | I | N   
       deriving Show 

    type Estado = Int

    type Transicion = (Estado, String, Estado, String, Mov)

-- --------------------------------------------------------------------------
-- 8 Finalmente incluirémos un tipo de dato para las máquinas de 
--   Turing y un tipo de dato para el resultado que obtengamos de
--   evaluar una máquina en particular con una palabra dada. 
-- --------------------------------------------------------------------------

    data Resultado = Acepta | Rechaza
       deriving Show
       
    type MT = ([Estado], [Char], [String], String, String, [Transicion], Estado, Estado, Estado)

-- --------------------------------------------------------------------------
-- 9 Comenzaremos por trabajar en una función que nos permita ver el 
--   contenido de un número determinado de celdas de la cinta en una 
--   configuración determinada.
--
--   Recordemos que cada configuración tiene acceso al contenido  
--   de la cinta en un instante de la ejecución. Como la cinta es 
--   infinita, si quisieramos imprimirla en pantalla directamente
--   nunca terminaríamos. 
--    
--   Esta función nos será de utilidad para revisar nuestro código
--   en los siguientes ejercicios. 
-- --------------------------------------------------------------------------

    {-Aquí va tu código-}

-- --------------------------------------------------------------------------
-- 10 El paso de una configuración a otra se da al aplicar una regla de
--   transición adecuada. 
--  
--   Una regla de transición es una quíntupla (q, c, p, s, m) perteneciente
--   a la función delta de la máquina de Turing, donde:
--     q es el estado actual de la unidad de control.
--     c es el contendio actual de la cinta.
--     p es el estado al que avanzamos.
--     s es el símbolo que escribimos en la cinta.
--     m es la instrucción de movimiento que seguirá la máquina: derecha, 
--          izquierda o ninguno. 
-- 
--   ¿Qué condiciones permiten aplicar una regla de transición a una
--   configuración? 
--  
--   Definamos una función que responda a esta pregunta:
-- --------------------------------------------------------------------------  

   {-Aquí va tu código-}

-- --------------------------------------------------------------------------  
-- 11 Con la función anteiror podemos seleccionar de entre las reglas definidas
--   en delta, la regla de transición adecuada para la configuración actual. 
--   
--   
--   Ya que nuestra máquina es determinista, no existirá más de una 
--   transición disponible para cada configuración, sin embargo, regresar
--   una lista de transiciones permite manejar los casos adversos en los que la
--   función delta no esté bien definida y para la configuración actual exista
--   más de una transición disponible o ninguna.  
--   
-- --------------------------------------------------------------------------  

    {-Aquí va tu código-}

-- -------------------------------------------------------------------------- 
-- 12 Con las funciones anteriores podemos escribir el cuerpo de la función
--   «aplica» que toma las reglas de transición en delta, la configuración 
--   actual de la máquina y regresa la configuración posterior inmediata 
--   en el proceso de ejecución de la máquina.  
-- -------------------------------------------------------------------------- 
    
    {-Aquí va tu código-}

-- -------------------------------------------------------------------------- 
-- 13 Si conocemos cuáles son los estados de aceptación y rechazo de 
--    nuestra máquina y su función de transición, podremos determinar si,
--    a partir de una configuración, llegaremos a rechazar o aceptar el 
--    contenido de la cinta. 
--
--    Definamos la función «aplicaRec» que evalúa sucesivamente la 
--    función «aplica» generando una secuencia de configuraciones hasta 
--    llegar a un estado de aceptación o rechazo. 
-- -------------------------------------------------------------------------- 

   
   {-Aquí va tu código-}


-- -------------------------------------------------------------------------- 
-- 14 Finalmente tenemos la función «eval» que recibirá la instancia de una 
--    máquina de Turing, una cadena y devolverá un resultado que indique si 
--    la máquina aceptó o rechazó dicha cadena. 
-- -------------------------------------------------------------------------- 
   

   {-Aquí va tu código-}

-- -------------------------------------------------------------------------- 
-- 15 Define la máquina de Turing que acepta el lenguaje con igual número de
--    a's, b's y c's ordenadas alfabéticamente. 
--
--    L(MT) = {w | w = a^n b^n c^n, n >= 0}
--
-- -------------------------------------------------------------------------- 

    
    {-Aquí va tu código-}

-- -------------------------------------------------------------------------- 
-- Aquí tenemos una función delta de prueba para revisar y corregir cualquier
-- posible error en el código
-- -------------------------------------------------------------------------- 

    deltaTest = [t0S, t0a, t0b, t0c, t0n, t1a, t1b, t1c, t1n, t2a, t2b, t2c, t2n, t3S, t3a, t3b, t3c, t3n,
                 t4S, t4a, t4b, t4c, t4n, t5S, t5a, t5b, t5n, t6S, t6a, t6n, t7a, t7b, t7c, t7n, t7f,
                t8a, t8b, t8c, t8n, t8f, t9b, t9c, t9n, t9f, t10c, t10n, t10f]

-- -------------------------------------------------------------------------- 
-- Los identificadores de cada regla hacen referencia al estado y al símbolo
-- que tomamos por entradas de la función delta.
-- 
-- Estos identificadores son de la forma tQx donde:
--            - Q es el estado en el que nos encontramos
--            - x es el símbolo que estamos leyendo
-- 
-- Cabe mencionar que:
--   S es para el marcador final "|-"
--   n es para el espacio en blanco  " "
--   f es para el nuevo símbolo "-|"
--
-- Además los estados t y r de aceptación y rechazo tendrán el número
-- 12 y 11 respectivamente.
--
-- Por otro lado, los guiones en la tabla de transiciones que define a delta
-- en el pdf son comodines (don't cares). No están incluídas las transiciones
-- que nos llevaban en su totalidad a un comodín, por ejemplo, el estado q6
-- leyendo b. 
-- -------------------------------------------------------------------------- 

    t0S = (0, "|-", 0, "|-", D) -- En el estado 0 leyendo el símbolo S ("|-")
    t0a = (0, "a", 0, "a", D)   -- En el estado 0 leyendo el símbolo a
    t0b = (0, "b", 1, "b", D)
    t0c = (0, "c", 2, "c", D)
    t0n = (0, "_", 3, "-|",I)   -- En el estado 0 leyendo el símbolo n ("-|")

    t1a = (1, "a", 11, "a", N)  -- los comodines preserbarán el símbolo en la cadena y no moverán la cabeza 
    t1b = (1, "b", 1, "b", D)
    t1c = (1, "c", 2, "c", D)
    t1n = (1, "_", 3, "-|",I)

    t2a = (2, "a", 11, "a", N) 
    t2b = (2, "b", 11, "b", N) 
    t2c = (2, "c", 2, "c", D)
    t2n = (2, "_", 3, "-|",I)

    t3S = (3, "|-", 12, "|-", N)
    t3a = (3, "a", 11, "a", N)
    t3b = (3, "b", 11, "b", N)
    t3c = (3, "c", 4, "_", I)
    t3n = (3, "_", 3, "_", I)

    t4S = (4, "|-", 11, "|-", N)
    t4a = (4, "a", 11, "a", N) 
    t4b = (4, "b", 5, "_", I) 
    t4c = (4, "c", 4, "c", I)
    t4n = (4, "_", 4, "_", I)

    t5S = (5, "|-", 11, "|-", N)
    t5a = (5, "a", 6, "_", I)
    t5b = (5, "b", 5, "b", I)
    t5n = (5, "_", 5, "_", I)

    t6S = (6, "|-", 7, "|-", D)
    t6a = (6, "a", 6, "a", I)
    t6n = (6, "_", 6, "_", I)

    t7a = (7, "a", 8, "_", D)
    t7b = (7, "b", 11, "b", N) 
    t7c = (7, "c", 11, "c", N) 
    t7n = (7, "_", 7, "_", D)
    t7f = (7, "-|", 12, "-|", N)

    t8a = (8, "a", 8, "a", D)
    t8b = (8, "b", 9, "_", D)
    t8c = (8, "c", 11, "c", N)
    t8n = (8, "_", 8, "_", D)
    t8f = (8, "-|", 11, "-|", N)
    
    t9b = (9, "b", 9, "b", D)
    t9c = (9, "c", 10, "_", D)
    t9n = (9, "_", 9, "_", D)
    t9f = (9, "-|", 11, "-|", N)

    t10c = (10, "c", 10, "c", D)
    t10n = (10, "_", 10, "_", D)
    t10f = (10, "-|", 3, "-|",I)

   
 
