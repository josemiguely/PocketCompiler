# Entrega 2

## Integrantes: Stevens Egli, José Miguel Yuseff

---

## Print

Se hace el llamado a funciones externas en el compilador usando en el encabezado del código Assembly la palabra clave `extern`. Aquí se crea una función `print` en **`sys.c`** similar a la función `print_res` creada anteriormente para imprimir el resultado de una compilación, pero con la diferencia de que agrega el carácter ">" cuando es llamada.

## Funciones de primer orden

Se decidió por usar la convención de llamado de x64. A continuación, un resumen de su implementación:

Para compilar funciones de primer orden, se generó primero la sintaxis para compilar programas, de tal forma que un programa queda definido como:
```ocaml 
prog : <fundef list> <expr>
```

De esta forma, nuestro punto de entrada en la compilación sería primero compilar un programa utilizando la siguiente función.
```ocaml 
compile_prog (p : prog) : string
```
La compilación de un programa, primero procederá a compilar la lista de funciones con:

```ocaml 
compile_list_fundef (flist : fundef list) []
```
Esta función compila cada función en la lista y devuelve el Sting de la compilación de funciones. El Sting resultante en código Assembly son labels de las funciones, con las que el programa puede saltar con el fin de ejecutar cada función que es llamada en la expresión del programa. 

En concreto, para compilar cada función de la lista de funciones se utiliza:
```ocaml 
compile_decl (fdef:fundef) (fun_env : funenv) : (string * funenv)
```
Esta función añade a un ambiente de funciones cada función de la lista, en conjunto con su cantidad de sus argumentos. Por otro lado, añade sus argumentos al ambiente de variables comunes. 

Para distinguir en el ambiente común entre los argumentos y variables locales se añadió el tipo `Kind`, que tiene como constructores `ArgKind` y `LocalKind`. 

Finalmente, ``compile_decl`` devuelve el nuevo ambiente de funciones, para que así el resto de funciones tengan en su ambiente de funciones las funciones que fueron declaradas previamente.


### Otras funciones relevantes en esta sección son:

```ocaml
let rec var_count(ex: tag expr) : int 
```
Esta función permite contar la cantidad de variables locales a utilizar en una expresión, para que así el RSP haga el espacio necesario para alojarlas.


### Otras cosas añadidas:

Se implementaron algunas funciones puras para modularizar el código.

## Objetivos extra

### Gestión de errores aritméticos

Se implementaron los siguientes operaciones en el lenguaje
- División entera ( / )
- Multiplicación ( * )
- Resta ( - )

Para manejar los errores aritméticos implementan 4 funciones en `C`. Tres funciones para terminar por over/under flow (superando en máximo valor numérico [MAXINT] o el mínimo [MININT]) y una para manejar la división por cero:

```c

void check_overflow_add (VAL a, VAL b)
void check_overflow_sub (VAL a, VAL b)
void check_overflow_mul (VAL a, VAL b)
void check_non_zero_denominator (VAL d)

``` 
Asumiendo que los valores extremos numéricos son:

**MININT** = -4611686018427387904 

**MAXINT** = 4611686018427387903

Las funciones llamadas desde `sys.c` hacen lo siguiente:
 
**```check_overflow_add(a,b)```** : Función que recibe 2 parámetros y compara, en el caso de que el flag *``-safe``* se escriba al ejecutar el programa, que se cumpla que:
- Si $a > \text{MAXINT} - b$ : entrega el error `"Arithmetic error: + produced an over flow"`
- Si $a < \text{MININT} - b$ : entega el error `"Arithmetic error: + produced an under flow"`
- En cualquier otro caso, la función no hace nada (no hay error)

**```check_overflow_sub(a,b)```** : Función que recibe 2 parámetros y compara, en el caso de que el flag *``-safe``* se escriba al ejecutar el programa, que se cumpla que:
- Si $a > \text{MAXINT} + b$ : entrega el error `"Arithmetic error: - produced an over flow"`
- Si $a < \text{MININT} + b$ : entega el error `"Arithmetic error: - produced an under flow"`
- En cualquier otro caso, la función no hace nada (no hay error)

**```check_overflow_mul(a,b)```** : Función que recibe 2 parámetros y compara, en el caso de que el flag *``-safe``* se escriba al ejecutar el programa, que se cumpla que:
- Si $a > \text{MAXINT} / b$ : entrega el error `"Arithmetic error: * produced an over flow"`
- Si $a < \text{MININT} / b$ : entega el error `"Arithmetic error: * produced an under flow"`
- En cualquier otro caso, la función no hace nada (no hay error).

Estas tres funciones guardan, a medida que se compila una expresión, dentro de los registros `RDI` y `RSI` la expresión ya compilada de la parte izquierda como el parámetro ``a`` y la derecha como el parámetro ``b``, evitando que se lleguen a operar antes de producir un desbordamiento.

**`check_non_zero_denominator(d)`**: Función que recibe 1 parámetro y compara, en el caso de que el flag *``-safe``* se escriba al ejecutar el programa, que:
- Si $d == 0$ : entreg el error `"Arithmetic error: Division by 0"`
- En cualquier otro caso, la función no hace nada (no hay error).

Esta función es llamada cuando se compila la expresión que **dividirá** a otra, haciendo uso del registro `RDI` para guardar el valor compilado de  *`d`*.

## Testing

Se mantienen los test de la entrega anterior y se agregan los respectivos para las operaciones agregadas en `Gestión de errores aritméticos` similares a los test de las operaciones ya implementadas

Por otro lado, se testean las nuevas implementaciones y errores:

### *Print*
- Se testea el corto circuito de la operación *and*
- Se testea prints con números, booleanos y funciones
- Se testan multiples prints en una expresión

### *Funciones de primer orden*
- Se testea una declaración de funciones con menos de 6 argumentos
- Se testean más de una declaración de función con menos de 6 argumentos 
- Se testean funciones con más de 6 argumentos
- Se testean funciones de más de 6 argumentos que se llamen entre ellas
- Se testean funciones de más de 6 argumentos que se llamen entre ellas y que al retornar al caller, este no pierda el valor de sus registros/argumentos.

### *Errores*
- Se testea el error en tiempo de ejecución al operar operaciones enteras con booleanos sobre `+`, `-`, `/` , `*`, `add1` y `sub1`
- Se testea el error en tiempo de ejecución de operar en `+` dos números y se produzca un *overflow* y un *underflow*
- Se testea el error en tiempo de ejecución de operar en `-` dos números y se produzca un *overflow* y un *underflow*
- Se testea el error en tiempo de ejecución sobre operar `add1` con un número y se produzca un *overflow*
- Se testea el error en tiempo de ejecución sobre operar `sub1` con un número y se produzca un *underflow*
- Se testea el error en tiempo de ejecución de operar en `/` dos números y se produzca una *división por 0*
- Se testea que la *aridad* de funciones sea correcta en tiempo de compilación
- Se testea que la función este *definida* al ser llamada en tiempo de compilación
