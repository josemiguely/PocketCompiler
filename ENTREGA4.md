# Entrega 4

## Integrantes: Stevens Egli, José Miguel Yuseff

---

## Implementación de Lambdas y Clausuras 

Se implementaron funciones de primera clase creando la noción de clausuras. Para crear una clausura se uso un heap pointer en el registro `R15` de nuestro compilador al igual como realizado para las tuplas. 

La clausura de una función posee la siguiente información:

| Aridad de la función | Puntero a sección de código de la función | Cantidad de variables libres | Variables Libre 1 | Variable Libre 2 | . . . 


Para realizar la compilación del cuerpo de un lambda realizamos un unpack de la clausura, copiando cada valor de las variables libres en el stack.

También se opto por el diseño de mantener un ambiente de funciones de segunda clase definidas con `def` separado del ambiente de funciones de primera clase definidas con `lambda (...)`.

Se crearon también las siguientes funciones para ayudar con la compilación de un labmda:

```freeVars```: Devuelve la cantidad de variables libres dentro de una expresión

```load_free_vars_to_stack```: Carga variables libres desde la clausura al stack

```get_slots```: Recupera los primeros slots en el ambiente

```add_free_vars_to_closure```: Agrega las variables libres a la clausura


## Otros cambios

Se refactorizo la expresión `Id` dentro de la función `compile_expr` junto a otras refactorizaciones de código usando funciones puras.

También se arreglaron todos los warnings del compilador de Ocaml.


## Testing

Se añaden test parar las clausuras en `bbctest/value/closures`, donde se imprimen de la forma *\<clos:n\>* con *n* la aridad de la lambda. Tambien son probadas con distintos tipos de datos y operaciones.

### *Funciones de primera clase*
En la carpeta `bbctest/lambdaApply` se prueba lo siguiente:
- Aplicar una lambda con argumentos
- Aplicar una lambda sin argumentos
- Aplicar una lambda con más de 6 argumentos
- Aplicar una lambda de una lambda
- Aplicar una lambda no comparte el mismo espacio de nombres que la funciones definidas por *def*
- Aplicar una lambda usando una lambda como argumento
- Aplicar lambdas con multiples operaciones.

### *Errores*
En la carpeta `bbctest/lambdaApply/run_time_errors` se prueban los siguientes errores:

- Se prueba que una aplicaion de lambda **no** pueda ser aplicada a un un *tupla*
- Se prueba que una aplicaion de lambda **no** pueda ser aplicada a un *booleano*
- Se prueba que una aplicaion de lambda **no** pueda ser aplicada a un *número*
- Se prueba que un lambda **no**  se aplicada en caso de que se le entregen menos argumentos que su aridad 
- Se prueba que un lambda **no** se aplicada en caso de que se le entregen más argumentos que su aridad 

La verificación de que una expresión es una *clausura* se agregó en la función `error` de `sys.c`. Los errores de aridad se hicieron con la función `closure_arity_mismatch` de `sys.c`.
