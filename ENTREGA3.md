# Entrega 3

## Integrantes: Stevens Egli, José Miguel Yuseff

---

## Implementación de tuplas

Se implementó tuplas utilizando el heap, el cual es proveído en el `main` de `sys.c`. Este se utiliza como un heap pointer en el registro `R15` de nuestro compilador. 

Se compilo la tupla de manera recursiva a través de sus elementos en sentido de izquierda-derecha. De esta forma en las tuplas anidadas, las direcciones de memoria que apuntan a otra tupla están en un espacio de memoria menor al que se encuentran.

Dado que no existe ANF los elementos de las tuplas fueron guardados en stack.

Para implementar las tuplas también se utilizarón las funciones auxiliares `add_tuple_to_heap` y `generate_list_env_slot`. La primera función  agrega cada compilación de una tupla al heap, mientras que la segunda genera una lista de pares de ambientes y slots en el stacks para poder asignar correctamente un slot a cada elementos de la tupla. 

Para realizar la operación `Get (expresion, n)` de una tupla simplemente se recuperó la tupla  y el índice al cual acceder en el heap a través de la compilación de `expresion` y `n` respectivamente.



## Mutación del heap

La mutación del heap se implementó pisando las direcciones de memoria con nuevos valores. Esto crea basura en los espacios en que la dirección para acceder a estos se haya reemplazado.

## Records

Se logró implementar los constructores para la declaración de un record:  
```‹decl›: ... | ( record recordId fieldId ... )``` 

También los constructores para la compilación de un record:
```‹expr›: ... | ( recordId ‹expr› ... ) | ( recordId-fieldId ‹expr› )```

También se extendió el parser con records en `parse.ml`. No se logró la implementar la compilación de records.

## Otros cambios

Se refactorizó el print en `sys.c` y varias funciones en `compile.ml`. También se implementaron más funciones puras en las secciones de código que hacia falta.

## Testing

Se mantienen los test de la entrega anterior: Se agregan test para el nuevo tipo de dato "tuple", un get para extraer un elemento de la posicion de un una tupla y un set que cambia una posición específica.

### *tupla*
- Se testea tuplas básicas de un número
- Se testea tuplas con multiples números
- Se testean tuplas con valores de distintos tipos 
- Se testean tuplas vacias

### *set*
- Se testea remplazar el valor de una tupla por un valor simple
- Se testea remplazar una posición de una tupla por otra tupla

### *get*
- Se testea extraer un valor simple de una tupla de un elemento 
- Se testea extraer un valor simple de una tupla con mas de un elemento
- Se testea extraer un valor no mumerico de una tupla
- Se testea extraer una tupla anidada de una tupla 

### *Errores*
- En *set* y *get* se testea el error en tiempo de ejecución que solamente set sea aplicable a una tupla
- En *set* y *get* se testea el error en tiempo de ejecución de acceder a un indice negativo de la una tupla
- En *set* y *get* se testea el error en tiempo de ejecución de acceder a un indice fuera del tamaño de la tupla.

La verificación de que una expresión es una tupla se agregó en la función `error` de `sys.c`. Los los errores de indice se hicieron con la función `tuple_index_error` en C.

