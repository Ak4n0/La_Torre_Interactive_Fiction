*>****************************************************************
*> Author:  Miquel A. Fuster Sancho
*> Date:    21-DIC-2021
*> Purpose: Profundizar en Cobol mediante un proyecto diferente
*> Tectonics: cobc
*> Disclaimer: Este código pretende ser una implementación del
*>  código de 'La Torre' aparecido en el tutorial de INFSP
*>  en la página del Club de Aventuras AD
*>  (ver web http://www.caad.es/informate/infsp/infsp6_docs.htm)
*>  La autoría de la aventura es de sus correspondientes autores
*>  originales. Este es solamente un ejercicio de programación
*>  sin ningún ánimo de lucro.
*>****************************************************************

IDENTIFICATION DIVISION.
PROGRAM-ID. LA-TORRE.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 LOCALIDADES OCCURS 8 INDEXED BY LOC-ID.
    05 NOMBRE-LOCALIDAD         PIC X(32).
    05 DESCRIPCION-LOCALIDAD    PIC X(255).
    05 DIRECCIONES.
        10 AL-N                 PIC 9.
        10 AL-S                 PIC 9.
        10 AL-E                 PIC 9.
        10 AL-O                 PIC 9.
        10 A-ARRIBA             PIC 9.
        10 A-ABAJO              PIC 9.
        10 A-ADENTRO            PIC 9.
        10 A-AFUERA             PIC 9.

01 OBJETOS OCCURS 13 INDEXED BY OBJ-ID.
    05 NOMBRE-OBJETO        PIC X(32).
    05 DESCRIPCION-OBJETO   PIC X(255).
    05 SINONIMOS            PIC X(32)   OCCURS 5 INDEXED BY SINONIMOS-ID.
    05 PERTENENCIA          PIC 9.
    05 ATRIBUTOS.
        10 INTERACTIVIDAD   PIC 9.
            88 ESTANDAR     VALUE 0.
            88 ESCENARIO    VALUE 1.
            88 OCULTO       VALUE 2.
        10 GENERO           PIC 9.
            88 MASCULINO    VALUE 0.
            88 FEMENINO     VALUE 1.
        10 NUMERO           PIC 9.
            88 SINGULAR     VALUE 0.
            88 PLURAL       VALUE 1.
        10 GENERAL          PIC 9.
            88 DESACTIVADO  VALUE 0.
            88 ACTIVADO     VALUE 1.

01 PALABRAS-PARSER.
    05 VERBO            PIC A(32).
    05 NOMBRE-1         PIC X(32).
    05 PREPOSICION      PIC A(32).
    05 NOMBRE-2         PIC X(32).

01 CADENA-DE-SALIDA.
    05 N-ESPACIOS-FINALES   PIC 999     USAGE COMP-3.
    05 LARGO-DE-CADENA      PIC 999     USAGE COMP-3.
    05 CADENA-SALIDA        PIC X(255).

77 SALIR-JUEGO              PIC 9.
77 ENTRADA-JUGADOR          PIC X(255).
77 VERBO-ID                 PIC 99.
77 OBJETO-1-ID              PIC 99.
77 OBJETO-2-ID              PIC 99.
77 LOCALIDAD-ACTUAL         PIC 9.
77 NUMERO-OBJETOS           PIC 99.

PROCEDURE DIVISION.

JUEGO SECTION.

MAIN-PROCEDURE.
    PERFORM MOSTRAR-TITULO.
    PERFORM INICIALIZAR-LOCALIDADES.
    PERFORM INICIALIZAR-OBJETOS.
    PERFORM INICIALIZAR-JUEGO.
    PERFORM DESCRIBIR-LOCALIDAD.
    PERFORM BUCLE-JUEGO UNTIL SALIR-JUEGO EQUALS 1.
STOP RUN.

BUCLE-JUEGO.
    IF LOCALIDAD-ACTUAL = 7 *> SE LLEGÓ AL OBJETIVO FINAL
        PERFORM REINICIAR-JUEGO
    ELSE
        PERFORM OBTENER-ENTRADA
        PERFORM PARSEAR
    END-IF.

MOSTRAR-TITULO.
    DISPLAY 'LA TORRE'.
    DISPLAY ' '
    DISPLAY 'Estás encerrado en una torre desde hace mucho tiempo.'.
    DISPLAY 'Nadie se acuerda ya de tí. Solo tú puedes obtener tu libertad.'.
    DISPLAY ' '.
    DISPLAY 'Pulsa una tecla para continuar...'.
    ACCEPT ENTRADA-JUGADOR.

TERMINAR-JUEGO.
    DISPLAY '¿Estás seguro que quieres terminar la partida? ' WITH NO ADVANCING.
    ACCEPT ENTRADA-JUGADOR.
    MOVE FUNCTION UPPER-CASE(ENTRADA-JUGADOR) TO ENTRADA-JUGADOR.
    IF ENTRADA-JUGADOR = 'S' OR 'SI'
        MOVE 1 TO SALIR-JUEGO
    END-IF.

REINICIAR-JUEGO.
    DISPLAY ' '.
    DISPLAY '¿Quieres jugar de nuevo? ' WITH NO ADVANCING.
    ACCEPT ENTRADA-JUGADOR.
    MOVE FUNCTION UPPER-CASE(ENTRADA-JUGADOR) TO ENTRADA-JUGADOR.
    IF ENTRADA-JUGADOR = 'S' OR 'SI'
        PERFORM MAIN-PROCEDURE
    ELSE
        MOVE 1 TO SALIR-JUEGO
    END-IF.

ESCRIBIR-CADENA.
    *> Imprime la cadena sin los espacios del final
    MOVE ZERO TO N-ESPACIOS-FINALES.
    *> cuenta cuantos espacios hay al final
    INSPECT FUNCTION REVERSE(CADENA-SALIDA) TALLYING N-ESPACIOS-FINALES FOR LEADING SPACES.
    *> resta los espacios al largo de la cadena
    SUBTRACT N-ESPACIOS-FINALES FROM LENGTH OF CADENA-SALIDA GIVING LARGO-DE-CADENA
    *> en LARGO-DE-CADENA está la cantidad de caracteres útiles de la cadena
    DISPLAY CADENA-SALIDA(1:LARGO-DE-CADENA) WITH NO ADVANCING.

ESCRIBIR-CADENA-CON-SALTO.
    PERFORM ESCRIBIR-CADENA.
    DISPLAY ' '.

ENUMERAR-OBJETOS-VISIBLES.
    PERFORM CUENTA-OBJETOS-VISIBLES.

    IF NUMERO-OBJETOS > 0
        DISPLAY 'Puedes ver ' WITH NO ADVANCING
        PERFORM VARYING OBJ-ID FROM 1 BY 1 UNTIL OBJ-ID > 13
            IF INTERACTIVIDAD(OBJ-ID) = 0 AND PERTENENCIA(OBJ-ID) = LOCALIDAD-ACTUAL
                EVALUATE TRUE
                    WHEN MASCULINO(OBJ-ID)
                        EVALUATE TRUE
                            WHEN SINGULAR(OBJ-ID)
                                DISPLAY 'un ' WITH NO ADVANCING
                            WHEN PLURAL(OBJ-ID)
                                DISPLAY 'unos ' WITH NO ADVANCING
                        END-EVALUATE
                    WHEN FEMENINO(OBJ-ID)
                        EVALUATE TRUE
                            WHEN SINGULAR(OBJ-ID)
                                DISPLAY 'una ' WITH NO ADVANCING
                            WHEN PLURAL(OBJ-ID)
                                DISPLAY 'unas ' WITH NO ADVANCING
                        END-EVALUATE
                END-EVALUATE
                MOVE NOMBRE-OBJETO(OBJ-ID) TO CADENA-SALIDA
                PERFORM ESCRIBIR-CADENA
                SUBTRACT 1 FROM NUMERO-OBJETOS
                IF NUMERO-OBJETOS > 0
                    DISPLAY ', ' WITH NO ADVANCING
                ELSE
                    DISPLAY '.'
                END-IF
            END-IF
        END-PERFORM
    END-IF.

CUENTA-OBJETOS-VISIBLES.
    INITIALIZE NUMERO-OBJETOS.
    PERFORM VARYING OBJ-ID FROM 1 BY 1 UNTIL OBJ-ID > 13
        IF INTERACTIVIDAD(OBJ-ID) = 0 AND PERTENENCIA(OBJ-ID) = LOCALIDAD-ACTUAL
            ADD 1 TO NUMERO-OBJETOS
        END-IF
    END-PERFORM.

CUENTA-OBJETOS-INVENTARIO.
    INITIALIZE NUMERO-OBJETOS.
    PERFORM VARYING OBJ-ID FROM 1 BY 1 UNTIL OBJ-ID > 13
        IF PERTENENCIA(OBJ-ID) = 8
            ADD 1 TO NUMERO-OBJETOS
        END-IF
    END-PERFORM.

CUENTA-SALIDAS.
    INITIALIZE NUMERO-OBJETOS.
    IF AL-N(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF AL-S(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF AL-E(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF AL-O(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF A-ARRIBA(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF A-ABAJO(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF A-ADENTRO(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.
    IF A-AFUERA(LOCALIDAD-ACTUAL) > 0
        ADD 1 TO NUMERO-OBJETOS
    END-IF.


INICIALIZACIONES SECTION.

INICIALIZAR-JUEGO.
    SET LOCALIDAD-ACTUAL TO 1.

INICIALIZAR-LOCALIDADES.
    SET LOC-ID TO 0.

*> 1. PUERTA PRINCIPAL
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Puerta Principal' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE 'Estás junto a la puerta principal. A su lado puedes ver una mesa de guardia y en la pared norte una chimenea.' TO DESCRIPCION-LOCALIDAD(LOC-ID).
    MOVE 2 TO AL-E(LOC-ID).

*> 2. DORMITORIO
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Dormitorio' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE 'Varios maltrechos catres se amontonan en esta habitación.' TO DESCRIPCION-LOCALIDAD(LOC-ID).
    MOVE 1 TO AL-O(LOC-ID).
    MOVE 3 TO AL-E(LOC-ID).

*> 3. ESCALERA DE CARACOL
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Escalera de caracol' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE 'El viento ulula a través de la empinada escalera de caracol, una vieja armadura parece vigilar la escalera.' TO DESCRIPCION-LOCALIDAD(LOC-ID).
    MOVE 5 TO A-ARRIBA(LOC-ID).
    MOVE 2 TO AL-O(LOC-ID).

*> 4. MAZMORRA
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Mazmorra' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE 'Una silenciona estancia débilmente alumbrada por los rayos de luna que se filtran a través de un pequeño ventanuco. El suelo está lleno de paja, colgando de unos grilletes en la pared observas un esqueleto humano.' TO DESCRIPCION-LOCALIDAD(LOC-ID).
    MOVE 5 TO AL-E(LOC-ID).

*> 5. ESCALERA DE CARALCOL 2
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Ecalera de caracol' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE 'Los desgastados peldaños de piedra resbalan en ocasiones. A mitad de la escalera una antorcha en la pared impide que la oscuridad sea completa.' TO DESCRIPCION-LOCALIDAD(LOC-ID).
    MOVE 3 TO A-ABAJO(LOC-ID).
    MOVE 6 TO A-ARRIBA(LOC-ID).

*> 6. ALTO TORRE
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Alto de la torre' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE 'Una gran cama preside la estancia, los guresos barrotes no permiten la salida por la ventana, aunque de todos modos estaría demasaido alta.' TO DESCRIPCION-LOCALIDAD(LOC-ID).
    MOVE 5 TO A-ABAJO(LOC-ID).

*> 7. EXTERIOR DE LA TORRE
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Exterior de la Torre' TO NOMBRE-LOCALIDAD(LOC-ID).
    MOVE '¡Al fin libre de esta prisión! ¿Qué nuevas aventuras te aguardan ahora?' TO DESCRIPCION-LOCALIDAD(LOC-ID).

*> 8. JUGADOR (PSEUDO-LOCALIZACIÓN PARA EL INVENTARIO)
    SET LOC-ID UP BY 1.
    INITIALIZE LOCALIDADES(LOC-ID).
    MOVE 'Jugador' TO NOMBRE-LOCALIDAD(LOC-ID).

INICIALIZAR-OBJETOS.
    SET OBJ-ID TO 0.

*> 1. CARBON
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'trozo de carbón'  TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Un trozo de negro carbón que parece haber sobrevivido al fuego.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'TROZO'            TO SINONIMOS(OBJ-ID, 1).
    MOVE 'CARBON'           TO SINONIMOS(OBJ-ID, 2).
    MOVE 'CARBÓN'           TO SINONIMOS(OBJ-ID, 3).
    MOVE 'CARBóN'           TO SINONIMOS(OBJ-ID, 4).

*> 2. CHIMENEA
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'chimenea'         TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Hace mucho tiempo que no arde fuego alguno en esta vieja chimenea.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'CHIMENEA'         TO SINONIMOS(OBJ-ID, 1).
    MOVE 'HOGAR'            TO SINONIMOS(OBJ-ID, 2).
    MOVE 1                  TO PERTENENCIA(OBJ-ID).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET FEMENINO(OBJ-ID)    TO TRUE.

*> 3. CATRES
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'catres'           TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'CATRES'           TO SINONIMOS(OBJ-ID, 1).
    MOVE 'CAMASTROS'        TO SINONIMOS(OBJ-ID, 2).
    MOVE 'Todos los catres han sido reducidos a una masa informe de madera, excepto uno que aún está bien conservado.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 2                  TO PERTENENCIA(OBJ-ID).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET PLURAL(OBJ-ID)      TO TRUE.

*> 4. CATRE
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'catre'    TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Una funda de tela cubre la cama.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'CATRE'    TO SINONIMOS(OBJ-ID, 1).
    MOVE 'CAMASTRO' TO SINONIMOS(OBJ-ID, 2).
    MOVE 2          TO PERTENENCIA(OBJ-ID).
    MOVE 1          TO INTERACTIVIDAD(OBJ-ID).

*> 5. FUNDA
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'funda'            TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'La funda parece bastante resistente.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'FUNDA'            TO SINONIMOS(OBJ-ID, 1).
    MOVE 'TELA'             TO SINONIMOS(OBJ-ID, 2).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET FEMENINO(OBJ-ID)    TO TRUE.

*> 6. CORREAS
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'correas'          TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Son unas correas de cuero que' TO DESCRIPCION-OBJETO(OBJ-ID). *> El final de la descripción depende de su estado interno.
    MOVE 'CORREAS'          TO SINONIMOS(OBJ-ID, 1).
    MOVE 'CORREA'           TO SINONIMOS(OBJ-ID, 2).
    MOVE 'CUERO'            TO SINONIMOS(OBJ-ID, 3).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET FEMENINO(OBJ-ID)    TO TRUE.
    SET PLURAL(OBJ-ID)      TO TRUE.

*> 7. PAJA
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'paja'             TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Paja húmeda e inútil.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'PAJA'             TO SINONIMOS(OBJ-ID, 1).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET FEMENINO(OBJ-ID)    TO TRUE.

*> 8. ESQUELETO
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'esqueleto'        TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Los huesos amarillentos, las cuencas vacías.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'ESQUELETO'        TO SINONIMOS(OBJ-ID, 1).
    MOVE 'HUMANO'           TO SINONIMOS(OBJ-ID, 2).
    MOVE 'MUERTO'           TO SINONIMOS(OBJ-ID, 3).
    MOVE 'CADAVER'          TO SINONIMOS(OBJ-ID, 4).
    MOVE 4                  TO PERTENENCIA(OBJ-ID).
    SET ESCENARIO(OBJ-ID)   TO TRUE.

*> 9. VENTANUCO
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'ventanuco'        TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'A través de los barrotes de este ventanuco puedes ver el exterior de la torre, iluminado por una increíble luna llena.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'VENTANUCO'    TO SINONIMOS(OBJ-ID, 1).
    MOVE 'VENTANA'      TO SINONIMOS(OBJ-ID, 2).
    MOVE 'TRAGALUZ'     TO SINONIMOS(OBJ-ID, 3).
    MOVE 4              TO PERTENENCIA(OBJ-ID).
    SET ESCENARIO(OBJ-ID) TO TRUE.

*> 10. BARROTES SOLIDOS
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'barrotes sólidos' TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Al examinar de cerca los barrotes de la ventana descubres uno que parece estar más flojo.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'BARROTES'         TO SINONIMOS(OBJ-ID, 1).
    MOVE 'BARRAS'           TO SINONIMOS(OBJ-ID, 2).
    MOVE 4                  TO PERTENENCIA(OBJ-ID).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET PLURAL(OBJ-ID)      TO TRUE.

*> 11. BARROTE FLOJO
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'barra'            TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Parece que este barrote podría quitarse con un poco de esfuerzo.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'BARROTE'          TO SINONIMOS(OBJ-ID, 1).
    MOVE 'BARRA'            TO SINONIMOS(OBJ-ID, 2).
    SET OCULTO(OBJ-ID)      TO TRUE.
    SET FEMENINO(OBJ-ID)    TO TRUE.

*> 12. ANTORCHA
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'antorcha'         TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Un tenue fuego bailotea a su extremo.'    TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'ANTORCHA'         TO SINONIMOS(OBJ-ID, 1).
    MOVE 'TEA'              TO SINONIMOS(OBJ-ID, 2).
    MOVE 5                  TO PERTENENCIA(OBJ-ID).
    SET ESCENARIO(OBJ-ID)   TO TRUE.
    SET FEMENINO(OBJ-ID)    TO TRUE.

*> 13. CUCHILLO
    SET OBJ-ID UP BY 1.
    INITIALIZE OBJETOS(OBJ-ID).
    MOVE 'pequeño cuchillo' TO NOMBRE-OBJETO(OBJ-ID).
    MOVE 'Un pequeño cuchillo cuchillo cubierto de herrumbre. No parece haberle servido de mucho a su malogrado poseedor.' TO DESCRIPCION-OBJETO(OBJ-ID).
    MOVE 'CUCHILLO' TO SINONIMOS(OBJ-ID, 1).
    MOVE 'PEQUEÑO'  TO SINONIMOS(OBJ-ID, 2).
    MOVE 'PUÑAL'    TO SINONIMOS(OBJ-ID, 3).
    MOVE 'HOJA'     TO SINONIMOS(OBJ-ID, 4).
    MOVE 'CUCHILLA' TO SINONIMOS(OBJ-ID, 5).

PARSER SECTION.

OBTENER-ENTRADA.
    DISPLAY ' '.
    INITIALIZE ENTRADA-JUGADOR.
    DISPLAY '>> ' WITH NO ADVANCING.
    ACCEPT ENTRADA-JUGADOR.

PARSEAR.
    *> CONVERTIR LA ENTRADA A MAYÚSCULAS
    MOVE FUNCTION UPPER-CASE(ENTRADA-JUGADOR) TO ENTRADA-JUGADOR.

    *> SEPARAR LA FRASE EN SUS DIVERSOS COMPONENTES
    UNSTRING ENTRADA-JUGADOR
        DELIMITED BY SPACE
        INTO VERBO, NOMBRE-1, PREPOSICION, NOMBRE-2
    END-UNSTRING.

    IF VERBO NOT = SPACES
        *> OBTENER ACCIONES Y OBJETOS
        PERFORM OBTENER-VERBO-ID

        *> EJECUTAR EJECUTAR ACCIONES
        EVALUATE VERBO-ID
            WHEN 1
                PERFORM TERMINAR-JUEGO
            WHEN 2
                PERFORM IR-AL-NORTE
            WHEN 3
                PERFORM IR-AL-SUR
            WHEN 4
                PERFORM IR-AL-ESTE
            WHEN 5
                PERFORM IR-AL-OESTE
            WHEN 6
                PERFORM IR-ARRIBA
            WHEN 7
                PERFORM IR-ABAJO
            WHEN 8
                PERFORM IR-ADENTRO
            WHEN 9
                PERFORM IR-AFUERA
            WHEN 10
                PERFORM OBTENER-NOMBRE-1
                PERFORM EXAMINAR
            WHEN 11
                PERFORM INVENTARIO
            WHEN 12
                PERFORM OBTENER-NOMBRE-1
                PERFORM COGER
            WHEN 13
                PERFORM OBTENER-NOMBRE-1
                PERFORM DEJAR
            WHEN 14
                PERFORM OBTENER-NOMBRE-1
                PERFORM EMPUJAR
            WHEN 15
                PERFORM OBTENER-NOMBRE-1
                PERFORM TIRAR-DE
            WHEN 16
                PERFORM OBTENER-NOMBRE-1
                PERFORM OBTENER-NOMBRE-2
                PERFORM CORTAR
            WHEN 17
                PERFORM OBTENER-NOMBRE-1
                PERFORM OBTENER-NOMBRE-2
                PERFORM ATAR
            WHEN 18
                PERFORM OBTENER-NOMBRE-1
                PERFORM DESATAR
            WHEN 19
                PERFORM MOSTRAR-SALIDAS
            WHEN 20
                PERFORM DESCRIBIR-LOCALIDAD
            WHEN OTHER
                DISPLAY 'Lo siento, no te entiendo.'
        END-EVALUATE
    END-IF.

OBTENER-VERBO-ID.
    INITIALIZE VERBO-ID.
    IF VERBO = 'TERMINA' OR 'TERMINAR'
        MOVE 1 TO VERBO-ID
    ELSE IF VERBO = 'N' OR 'NORTE'
        MOVE 2 TO VERBO-ID
    ELSE IF VERBO = 'S' OR 'SUR'
        MOVE 3 TO VERBO-ID
    ELSE IF VERBO = 'E' OR 'ESTE'
        MOVE 4 TO VERBO-ID
    ELSE IF VERBO = 'O' OR 'OESTE'
        MOVE 5 TO VERBO-ID
    ELSE IF VERBO = 'SUBE' OR 'SUBIR'
        MOVE 6 TO VERBO-ID
    ELSE IF VERBO = 'BAJA' OR 'BAJAR'
        MOVE 7 TO VERBO-ID
    ELSE IF VERBO = 'ENTRA' OR 'ENTRAR'
        MOVE 8 TO VERBO-ID
    ELSE IF VERBO = 'SAL' OR 'SALTE' OR 'SALIR'
        MOVE 9 TO VERBO-ID
    ELSE IF VERBO = 'EX' OR 'EXAMINA' OR 'EXAMINAR'
        MOVE 10 TO VERBO-ID
    ELSE IF VERBO = 'I' OR 'INVENTARIO'
        MOVE 11 TO VERBO-ID
    ELSE IF VERBO = 'COGE' OR 'COGER' OR 'TOMA' OR 'TOMAR'
        MOVE 12 TO VERBO-ID
    ELSE IF VERBO = 'DEJA' OR 'DEJAR' OR 'SUELTA' OR 'SOLTAR'
        MOVE 13 TO VERBO-ID
    ELSE IF VERBO = 'EMPUJA' OR 'EMPUJAR'
        MOVE 14 TO VERBO-ID
    ELSE IF VERBO = 'TIRA' OR 'TIRAR' OR 'JALA' OR 'JALAR'
        MOVE 15 TO VERBO-ID
    ELSE IF VERBO = 'CORTA' OR 'CORTAR'
        MOVE 16 TO VERBO-ID
    ELSE IF VERBO = 'ATA' OR 'ATAR'
        MOVE 17 TO VERBO-ID
    ELSE IF VERBO = 'DESATA' OR 'DESATAR'
        MOVE 18 TO VERBO-ID
    ELSE IF VERBO = 'X' OR 'SALIDAS'
        MOVE 19 TO VERBO-ID
    ELSE IF VERBO = 'M' OR 'MIRA' OR 'MIRAR'
        MOVE 20 TO VERBO-ID
    END-IF.

OBTENER-NOMBRE-1.
    INITIALIZE OBJETO-1-ID, OBJ-ID.

    IF VERBO-ID = 15        *> CASO ESPECIAL DEL VERBO TIRAR,
        IF NOMBRE-1 = 'DE'  *> QUE SUELE LLEVAR LA PREPOSICION 'DE' ANTES DEL OBJETO DIRECTO.
            MOVE PREPOSICION TO NOMBRE-1 *> EL NOMBRE SE HABRÍA CARGADO EN PREPOSICION,
        END-IF                           *> DE ESTA MANERA SE TRANSPORTA A NOMBRE-1
    END-IF

    IF NOMBRE-1 NOT = SPACES
        PERFORM VARYING OBJ-ID FROM 1 BY 1 UNTIL OBJ-ID>13 OR OBJETO-1-ID > 0
            PERFORM VARYING SINONIMOS-ID FROM 1 BY 1 UNTIL SINONIMOS-ID > 5 OR OBJETO-1-ID > 0
                IF SINONIMOS(OBJ-ID, SINONIMOS-ID) = NOMBRE-1
                    MOVE OBJ-ID TO OBJETO-1-ID
                END-IF
            END-PERFORM
        END-PERFORM
    END-IF.

OBTENER-NOMBRE-2.
    INITIALIZE OBJETO-2-ID, OBJ-ID.

    IF VERBO-ID = 16                                        *> CASO ESPECIAL DEL VERBO CORTAR,
        IF PREPOSICION NOT = 'CON' AND NOMBRE-2 = SPACES    *> QUE SUELE LLEVAR LA PREPOSICION 'CON' ANTES DEL OBJETO INDIRECTO.
            MOVE PREPOSICION TO NOMBRE-2                    *> SI 'CON' NO ESTÁ EN SU LUGAR EL JUGADOR HA PUESTO EN 'PREPOSICION'
        END-IF                                              *> EL OBJETO INDIRECTO DE LA ORACION
    END-IF

    IF VERBO-ID = 17
        IF PREPOSICION NOT = 'A' AND NOMBRE-2 = SPACES
            MOVE PREPOSICION TO NOMBRE-2
        END-IF
    END-IF

    IF NOMBRE-2 NOT = SPACES
        PERFORM VARYING OBJ-ID FROM 1 BY 1 UNTIL OBJ-ID>13 OR OBJETO-2-ID > 0
            PERFORM VARYING SINONIMOS-ID FROM 1 BY 1 UNTIL SINONIMOS-ID > 5 OR OBJETO-2-ID > 0
                IF SINONIMOS(OBJ-ID, SINONIMOS-ID) = NOMBRE-2
                    MOVE OBJ-ID TO OBJETO-2-ID
                END-IF
            END-PERFORM
        END-PERFORM
    END-IF.
EJECUTAR-VERBOS SECTION.

DESCRIBIR-LOCALIDAD.
    DISPLAY ' '.
    DISPLAY '[ ' WITH NO ADVANCING.
    MOVE NOMBRE-LOCALIDAD(LOCALIDAD-ACTUAL) TO CADENA-SALIDA.
    PERFORM ESCRIBIR-CADENA.
    DISPLAY ' ]'.
    MOVE DESCRIPCION-LOCALIDAD(LOCALIDAD-ACTUAL) TO CADENA-SALIDA
    PERFORM ESCRIBIR-CADENA-CON-SALTO.
    PERFORM ENUMERAR-OBJETOS-VISIBLES.

IR-AL-NORTE.
    IF AL-N(LOCALIDAD-ACTUAL) > 0
        MOVE AL-N(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-AL-SUR.
    IF AL-S(LOCALIDAD-ACTUAL) > 0
        MOVE AL-S(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-AL-ESTE.
    IF AL-E(LOCALIDAD-ACTUAL) > 0
        MOVE AL-E(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-AL-OESTE.
    IF AL-O(LOCALIDAD-ACTUAL) > 0
        MOVE AL-O(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-ARRIBA.
    IF A-ARRIBA(LOCALIDAD-ACTUAL) > 0
        MOVE A-ARRIBA(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-ABAJO.
    IF A-ABAJO(LOCALIDAD-ACTUAL) > 0
        MOVE A-ABAJO(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-ADENTRO.
    IF A-ADENTRO(LOCALIDAD-ACTUAL) > 0
        MOVE A-ADENTRO(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

IR-AFUERA.
    IF A-AFUERA(LOCALIDAD-ACTUAL) > 0
        MOVE A-AFUERA(LOCALIDAD-ACTUAL) TO LOCALIDAD-ACTUAL
        PERFORM DESCRIBIR-LOCALIDAD
    ELSE
        DISPLAY 'No puedes ir por ahí.'
    END-IF.

MOSTRAR-SALIDAS.
    PERFORM CUENTA-SALIDAS.
    IF NUMERO-OBJETOS > 0
        DISPLAY 'Las salidas posibles son: ' WITH NO ADVANCING
        IF AL-N(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'norte' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF AL-S(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'sur' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF AL-E(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'este' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF AL-O(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'oeste' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF A-ARRIBA(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'subir' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF A-ABAJO(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'bajar' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF A-ADENTRO(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'entrar' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
        IF A-AFUERA(LOCALIDAD-ACTUAL) > 0
            DISPLAY 'salir' WITH NO ADVANCING
            PERFORM MOSTRAR-SALIDAS-MAS
        END-IF
    ELSE
        DISPLAY 'No hay salida.'
    END-IF.

MOSTRAR-SALIDAS-MAS.
    SUBTRACT 1 FROM NUMERO-OBJETOS
    IF NUMERO-OBJETOS > 1
        DISPLAY ', ' WITH NO ADVANCING
    ELSE IF NUMERO-OBJETOS = 1
        DISPLAY ' y ' WITH NO ADVANCING
    ELSE
        DISPLAY '.'
    END-IF.

EXAMINAR.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿Qué quieres examinar?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = LOCALIDAD-ACTUAL OR 8
        *> DESCRIBIR EL OBJETO
        MOVE DESCRIPCION-OBJETO(OBJETO-1-ID) TO CADENA-SALIDA
        PERFORM ESCRIBIR-CADENA

        *> DIFERENTES OBJETOS CON TRATAMIENTOS ESPECIALES
        EVALUATE OBJETO-1-ID

            WHEN 2 *> CHIMENEA
                *> SI ES LA PRIMERA VEZ QUE SE EXAMINA LA CHIMENEA
                EVALUATE TRUE WHEN DESACTIVADO(OBJETO-1-ID)
                    *> DAR LA CHIMENEA POR EXAMINADA
                    SET ACTIVADO(OBJETO-1-ID) TO TRUE
                    *> INFORMAR DEL CARBÓN EN LA DESCRIPCIÓN
                    DISPLAY ' Un trozo de carbón es todo lo que queda del antiguo hogar.' WITH NO ADVANCING
                    *> MOVER EL CARBÓN A LA MISMA HABITACIÓN QUE LA CHIMENEA.
                    MOVE PERTENENCIA(OBJETO-1-ID) TO PERTENENCIA(1)
                END-EVALUATE

            WHEN 4 *> CATRE
                *> SI ES LA PRIMERA VEZ QUE SE EXAMINAN EL CATRE
                EVALUATE TRUE WHEN DESACTIVADO(OBJETO-1-ID)
                    *> DAR A CATRE POR EXAMINADOS
                    SET ACTIVADO(OBJETO-1-ID) TO TRUE
                    *> TRAER FUNDA Y CORREAS AL DORMITORIO
                    MOVE PERTENENCIA(OBJETO-1-ID) TO PERTENENCIA(5), PERTENENCIA(6)
                END-EVALUATE

            WHEN 6 *> CORREAS
                EVALUATE TRUE
                    WHEN DESACTIVADO(OBJETO-1-ID)
                        DISPLAY ' sujetan la funda a la cama.' WITH NO ADVANCING
                    WHEN ACTIVADO(OBJETO-1-ID)
                        DISPLAY ' cuelgan de la cama.' WITH NO ADVANCING
                END-EVALUATE

            WHEN 8 *> ESQUELETO
                EVALUATE TRUE WHEN DESACTIVADO(OBJETO-1-ID)
                    SET ACTIVADO(OBJETO-1-ID) TO TRUE
                    DISPLAY ' Junto a él ves un pequeño cuchillo.' WITH NO ADVANCING
                    MOVE PERTENENCIA(OBJETO-1-ID) TO PERTENENCIA(13)
                END-EVALUATE

            WHEN 10 *> BARROTES
                EVALUATE TRUE WHEN DESACTIVADO(OBJETO-1-ID)
                    SET ACTIVADO(OBJETO-1-ID) TO TRUE
                    MOVE PERTENENCIA(OBJETO-1-ID) TO PERTENENCIA(11) *> MUEVE EL BARROTE FLOJO A LA HABITACIÓN
                END-EVALUATE

        END-EVALUATE
        *> REALIZAR EL SALTO DE LINEA
        DISPLAY ' '
    ELSE
        DISPLAY 'No veo eso que dices.'
    END-IF.

INVENTARIO.
    PERFORM CUENTA-OBJETOS-INVENTARIO.

    IF NUMERO-OBJETOS > 0
        DISPLAY 'Tienes ' WITH NO ADVANCING
        PERFORM VARYING OBJ-ID FROM 1 BY 1 UNTIL OBJ-ID > 13
            IF PERTENENCIA(OBJ-ID) = 8
                EVALUATE TRUE
                    WHEN MASCULINO(OBJ-ID)
                        EVALUATE TRUE
                            WHEN SINGULAR(OBJ-ID)
                                DISPLAY 'un ' WITH NO ADVANCING
                            WHEN PLURAL(OBJ-ID)
                                DISPLAY 'unos ' WITH NO ADVANCING
                        END-EVALUATE
                    WHEN FEMENINO(OBJ-ID)
                        EVALUATE TRUE
                            WHEN SINGULAR(OBJ-ID)
                                DISPLAY 'una ' WITH NO ADVANCING
                            WHEN PLURAL(OBJ-ID)
                                DISPLAY 'unas ' WITH NO ADVANCING
                        END-EVALUATE
                END-EVALUATE
                MOVE NOMBRE-OBJETO(OBJ-ID) TO CADENA-SALIDA
                PERFORM ESCRIBIR-CADENA
                SUBTRACT 1 FROM NUMERO-OBJETOS
                IF NUMERO-OBJETOS > 0
                    DISPLAY ', ' WITH NO ADVANCING
                ELSE
                    DISPLAY '.'
                END-IF
            END-IF
        END-PERFORM
    ELSE
        DISPLAY 'No tienes nada.'
    END-IF.

COGER.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = 8
        DISPLAY 'Ya tienes ' WITH NO ADVANCING
        EVALUATE TRUE
            WHEN MASCULINO(OBJETO-1-ID)
                EVALUATE TRUE
                    WHEN SINGULAR(OBJETO-1-ID)
                        DISPLAY 'el ' WITH NO ADVANCING
                    WHEN PLURAL(OBJETO-1-ID)
                        DISPLAY 'los ' WITH NO ADVANCING
                END-EVALUATE
            WHEN FEMENINO(OBJETO-1-ID)
                EVALUATE TRUE
                    WHEN SINGULAR(OBJETO-1-ID)
                        DISPLAY 'la ' WITH NO ADVANCING
                    WHEN PLURAL(OBJETO-1-ID)
                        DISPLAY 'las ' WITH NO ADVANCING
                END-EVALUATE
        END-EVALUATE
        DISPLAY NOMBRE-OBJETO(OBJETO-1-ID), '.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = LOCALIDAD-ACTUAL

        *> DIFERENTES OBJETOS CON TRATAMIENTOS ESPECIALES
        EVALUATE OBJETO-1-ID

            WHEN 5 *> FUNDA
                IF PERTENENCIA(OBJETO-1-ID) = 2 *> SOLO SI LA FUNDA ESTÁ EN EL DORMITORIO
                    EVALUATE TRUE WHEN ACTIVADO(4) *> SI LA CAMA NO SE HA DESTAPADO
                        EVALUATE TRUE
                            WHEN ACTIVADO(6) *> SI LAS CORREAS ESTAN CORTADAS
                                *> HACER DE LA FUNDA UN OBJETO ESTANDAR
                                SET ESTANDAR(OBJETO-1-ID) TO TRUE
                                *> DESTAPAR LA CAMA
                                SET ACTIVADO(4) TO TRUE
                                *> TRAE LA PAJA AL DORMITORIO
                                MOVE 2 TO PERTENENCIA(7)
                                *> CAMBIA LA DESCRIPCION DEL CATRE AL LLEVARTE LA FUNDA
                                INITIALIZE DESCRIPCION-OBJETO(4)
                                MOVE 'Sólo restos de paja cubren la cama.' TO DESCRIPCION-OBJETO(4)
                            WHEN DESACTIVADO(6) *> LAS CORREAS NO ESTÁN CORTADAS AÚN
                                DISPLAY 'La funda está sujeta a la cama por unas correas.'
                        END-EVALUATE
                    END-EVALUATE
                END-IF

            WHEN 11 *> BARROTE FLOJO
                IF PERTENENCIA(OBJETO-1-ID) = 4 *> SOLO SI EL BARROTE ESTÁ EN LA MAZMORRA
                    EVALUATE TRUE WHEN DESACTIVADO(OBJETO-1-ID) *> SI LA BARRA NO SE HA SACADO DE LOS BARROTES
                        DISPLAY 'Con un fuerte tirón arrancas el barrote del ventanuco.'
                        *> CAMBIAR LA DESCRIPCIÓN DE LOS BARROTES
                        INITIALIZE DESCRIPCION-OBJETO(10)
                        MOVE 'En la ventana falta un barrote. Parece que podrías pasar por el hueco.' TO DESCRIPCION-OBJETO(10)
                        *> CAMBIAR LA DESCRIPCIÓN DEL BARROTE
                        INITIALIZE DESCRIPCION-OBJETO(OBJETO-1-ID)
                        MOVE 'Una barra de hierro medio oxidada.' TO DESCRIPCION-OBJETO(OBJETO-1-ID)
                        *> DAR UNA INTERACTIVIDA ESTÁNDAR AL BARROTE
                        SET ESTANDAR(OBJETO-1-ID) TO TRUE
                        *> MARCA COMO MOVIDO EL BARROTE
                        SET ACTIVADO(OBJETO-1-ID) TO TRUE
                        *> SI LA FUNDA ESTÁ ATADA EL JUGADOR YA PUEDE SALIR DE LA TORRE
                        EVALUATE TRUE WHEN ACTIVADO(5)
                            MOVE 7 TO A-AFUERA(LOCALIDAD-ACTUAL) *> HACE QUE EL JUGADOR PUEDA 'SALIR' POR EL VENTANUCO
                            DISPLAY 'Tu salida hacia la libertad está lista.'
                        END-EVALUATE
                    END-EVALUATE
                END-IF

        END-EVALUATE
        *> REALIZAR EL SALTO DE LINEA

        *> SOLO COGE LOS OBJETOS QUE TENGA UNA INTERACTIVIDAD ESTÁNDAR
        EVALUATE TRUE
            WHEN ESTANDAR(OBJETO-1-ID) OR OCULTO(OBJETO-1-ID)
                MOVE 8 TO PERTENENCIA(OBJETO-1-ID)
                DISPLAY 'Ok.'
            WHEN OTHER
                DISPLAY 'No puedes llevarte eso.'
        END-EVALUATE
    ELSE
        DISPLAY 'No veo eso que dices.'
    END-IF.

DEJAR.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = 8
        MOVE LOCALIDAD-ACTUAL TO PERTENENCIA(OBJETO-1-ID)
        DISPLAY 'Ok.'
    ELSE
        DISPLAY 'No tienes ese objeto en tu inventario.'
    END-IF.

EMPUJAR.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = LOCALIDAD-ACTUAL OR 8
        EVALUATE OBJETO-1-ID

            WHEN 12 *> ANTORCHA
                IF AL-O(LOCALIDAD-ACTUAL) = ZERO *> SI NO ESTÁ ABIERTO EL PASO AL OESTE
                    MOVE 4 TO AL-O(LOCALIDAD-ACTUAL)
                    DISPLAY 'Al empujar la antorcha una porción de pared se abre al oeste dando acceso a una estancia.'
                ELSE *> EL PASO AL OESTE YA ESTÁ ABIERTO
                    DISPLAY 'La antorcha no cede más.'
                END-IF

            WHEN OTHER
                DISPLAY 'No tiene sentido hacer eso.'

        END-EVALUATE
    END-IF.

TIRAR-DE.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = LOCALIDAD-ACTUAL OR 8
        EVALUATE OBJETO-1-ID

            WHEN 12 *> ANTORCHA
                IF AL-O(LOCALIDAD-ACTUAL) = 4 *> SI ESTÁ ABIERTO EL PASO AL OESTE
                    MOVE ZERO TO AL-O(LOCALIDAD-ACTUAL)
                    DISPLAY 'Al tirar de la antorcha la puerta secreta se cierra de nuevo.'
                ELSE *> EL PASO AL OESTE YA ESTÁ CERRADO
                    DISPLAY 'La antorcha no cede más.'
                END-IF

            WHEN OTHER
                DISPLAY 'No tiene sentido hacer eso.'

        END-EVALUATE
    END-IF.

CORTAR.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = LOCALIDAD-ACTUAL OR 8
        EVALUATE OBJETO-1-ID

            WHEN 6 *> CORREAS
                EVALUATE OBJETO-2-ID
                    WHEN ZEROES
                        DISPLAY '¿Con qué quieres cortar?'
                    WHEN 13 *> CUCHILLO
                        IF PERTENENCIA(OBJETO-2-ID) = 8 *> EL CUCHILLO ESTÁ EN EL INVENTARIO
                            EVALUATE TRUE
                                WHEN DESACTIVADO(OBJETO-1-ID) *> LAS CORREAS TODAVÍA NO SE CORTARON
                                    SET ACTIVADO(OBJETO-1-ID) TO TRUE *> ACTIVA EL FLAG GENERAL DE LAS CORRAS PARA INDICAR QUE ESTÁN CORTADAS
                                    SET ESTANDAR(5) TO TRUE *> CONVIERTE LA FUNDA EN UN OBJETO ESTÁNDAR
                                    DISPLAY 'Cortas las correas con el cuchillo.'
                                WHEN ACTIVADO(OBJETO-1-ID) *> YA FUERON CORTADAS
                                    DISPLAY 'Ya las cortaste.'
                            END-EVALUATE
                        ELSE
                            DISPLAY '¡Vaya! ¿Y el cuchillo?'
                        END-IF

                    WHEN OTHER
                        DISPLAY 'Eso no corta.'

                END-EVALUATE

            WHEN OTHER
                DISPLAY 'Una acción totalemnte inútil, me temo.'

        END-EVALUATE
    ELSE
        DISPLAY 'No veo eso que dices.'
    END-IF.

ATAR.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = 8
        EVALUATE OBJETO-1-ID

            WHEN 5 *> FUNDA
                EVALUATE OBJETO-2-ID
                    WHEN ZEROES
                        DISPLAY '¿A qué quieres atarla?'
                    WHEN 10 *> BARROTES FIJOS
                        IF LOCALIDAD-ACTUAL = 4 *> EL JUGADOR DEBE ESTAR EN LA MAZMORRA
                            EVALUATE TRUE WHEN DESACTIVADO(OBJETO-1-ID) *> LA FUNDA NO ESTÁ ATADA
                                EVALUATE TRUE
                                    WHEN DESACTIVADO(11) *> EL BARROTE NO SE HA QUITADO
                                        DISPLAY 'Aún que ataras la funda allí no tienes suficiente espacio para pasar por los barrotes.'
                                    WHEN ACTIVADO(11) *> EL BARROTE SE HA QUITADO
                                        SET ACTIVADO(OBJETO-1-ID) TO TRUE *> SE ASIGNA EL FLAG PERA MARCAR QUE LA FUNDA ESTÁ ATADA
                                        SET ESCENARIO(OBJETO-1-ID) TO TRUE *> INTERACTUACIÓN LIMITADA CON LA FUNDA
                                        MOVE 4 TO PERTENENCIA(OBJETO-1-ID) *> LA FUNDA PASA A SER PARTE DE LA HABITACIÓN
                                        MOVE 7 TO A-AFUERA(LOCALIDAD-ACTUAL) *> HACE QUE EL JUGADOR PUEDA 'SALIR' POR EL VENTANUCO
                                        *> CAMBIA LA DESCRIPCIÓN DE LOS BARROTES
                                        INITIALIZE DESCRIPCION-OBJETO(10)
                                        MOVE 'En la ventana falta un barrote. Parece que podrías pasar por el hueco. La funda está atada por el exterior a modo de cuerda de escape.' TO DESCRIPCION-OBJETO(10)
                                        DISPLAY 'Tu salida hacia la libertad está lista.'
                                END-EVALUATE
                            END-EVALUATE
                        ELSE
                            DISPLAY 'No ves eso aquí.'
                        END-IF

                    WHEN 11 *> BARROTE FLOJO
                        IF LOCALIDAD-ACTUAL = 4
                            EVALUATE TRUE
                                WHEN DESACTIVADO(11) *> EL BARROTE FLOJO TODAVÍA NO SE HA QUITADO DEL VENTANUCO
                                    DISPLAY 'Mejor atarla a un barrote sólido.'
                                WHEN ACTIVADO(11)
                                    DISPLAY '¿Quieres hacerte un columpio? ¿En una mazmorra? Tus gustos son muy extraños. Mejor no.'
                            END-EVALUATE
                        ELSE
                            DISPLAY 'No te serviría de nada hacer eso.'
                        END-IF

                    WHEN OTHER
                        DISPLAY 'Cuando vas a realizar el lazo te das cuenta que no te servirá tener eso atado.'

                END-EVALUATE

            WHEN OTHER
                DISPLAY 'Eso no se usa para atar cosas.'

        END-EVALUATE
    ELSE
        DISPLAY 'No tienes eso.'
    END-IF.

DESATAR.
    IF NOMBRE-1 = SPACES
        DISPLAY '¿El qué?'
    ELSE IF OBJETO-1-ID = ZEROES
        DISPLAY 'Realmente desconozco a lo que te refieres.'
    ELSE IF PERTENENCIA(OBJETO-1-ID) = LOCALIDAD-ACTUAL
        EVALUATE OBJETO-1-ID

            WHEN 5 *> FUNDA
                IF LOCALIDAD-ACTUAL = 4 *> EL JUGADOR DEBE ESTAR EN LA MAZMORRA
                    EVALUATE TRUE
                        WHEN ACTIVADO(OBJETO-1-ID) *> LA FUNDA ESTÁ ATADA
                            SET DESACTIVADO(OBJETO-1-ID) TO TRUE *> SE ASIGNA EL FLAG PERA MARCAR QUE LA FUNDA ESTÁ DESATADA
                            SET ESTANDAR(OBJETO-1-ID) TO TRUE *> INTERACTUACIÓN PLENA CON LA FUNDA
                            MOVE 0 TO A-AFUERA(LOCALIDAD-ACTUAL) *> IMPIDE QUE EL JUGADOR PUEDA 'SALIR' POR EL VENTANUCO
                            DISPLAY 'Desatas la funda que queda en el suelo.'
                            *> CAMBIA LA DESCRIPCIÓN DE LOS BARROTES
                            INITIALIZE DESCRIPCION-OBJETO(10)
                            MOVE 'En la ventana falta un barrote. Parece que podrías pasar por el hueco.' TO DESCRIPCION-OBJETO(10)
                        WHEN DESACTIVADO(OBJETO-1-ID) *> LA FUNDA NO ESTÁ ATADA
                            DISPLAY 'La funda no está atada a ningún sitio.'
                    END-EVALUATE
                ELSE IF LOCALIDAD-ACTUAL = 2 *> EL JUGADOR ESTÁ EN EL DORMITORIO
                    EVALUATE TRUE
                        WHEN DESACTIVADO(6) *> LAS CORREAS NO ESTÁN CORTADAS
                            DISPLAY 'No tienes suficiente fuerza para desatar las correas. Deberías encontrar una mejor forma para soltarlas.'
                        WHEN ACTIVADO(6) *> LAS CORREAS YA SE CORTARON
                            DISPLAY 'La funda no está atada a nada.'
                    END-EVALUATE
                ELSE
                    DISPLAY 'La funda no está atada a nada.'
                END-IF

            WHEN 6 *> CORREAS
                IF LOCALIDAD-ACTUAL = 2 *> EL JUGADOR DEBE ESTAR EN EL DORMITORIO
                    EVALUATE TRUE
                        WHEN DESACTIVADO(OBJETO-1-ID) *> LAS CORREAS NO ESTÁN CORTADAS
                            DISPLAY 'No tienes suficiente fuerza en los dedos cómo para desatarlas. Deberás encontrar una mejor forma para soltarlas.'
                        WHEN ACTIVADO(OBJETO-1-ID) *> LAS CORREAS YA SE HAN CORTADO
                            DISPLAY 'Ya las cortaste, no es necesario deshatarlas.'
                    END-EVALUATE
                ELSE
                    DISPLAY 'No ves eso aquí.'
                END-IF

            WHEN OTHER
                DISPLAY 'Eso no está atado a nada.'

        END-EVALUATE
    ELSE
        DISPLAY 'No ves eso aquí.'
    END-IF.

END PROGRAM LA-TORRE.
