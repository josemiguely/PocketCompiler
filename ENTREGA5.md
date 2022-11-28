# Entrega 4

## Integrantes: Stevens Egli, José Miguel Yuseff

---

## Implementación de un GC 

Se uso el algoritmo de Cheney's para implementar un garbage collector. Para esto se agrego al compilador una llamada a la función de C `try_gc` cada vez que se solicitó memoria en el heap. En el caso que al pedir memoria el heap este lleno se ejecuta la recolección de basura. También se añadió la función `set_stack_bottom` que setea el bottom del stack.

Para más información del algoritmo visitar:
https://en.wikipedia.org/wiki/Cheney%27s_algorithm

No se tomo ninguna decisión de diseño importante, solamente se agrego un nuevo identificador para las direcciones que ya fueron movidas del FROM SPACE y TO SPACE con el valor `FORWARDED_TAG = Ox7`.


## Otros cambios

Se refactorizo algunas partes del código con funciones puras.


## Testing

### *GC para tuplas*
En la carpeta `bbctest/gc/tuplas` se prueba lo siguiente:

- Se testea que un programa se falle al declarar una tupla de tamaño mayor al declarado para el heap sin usar GC.
- Se testea que un programa de una tupla, que no sobrepase el límite de tamaño del heap, funcione correctamente sin usar GC. 
- Se testea que un programa, al utilizar múltiples tuplas que suman un tamaño mayor al declarado del heap, no falle dado que usa GC.
- Se testea que un programa, al utilizar múltiples tuplas que suman un tamaño mayor al declarado del heap, falle dado que **NO** usa GC.

### *GC para lambdas*
En la carpeta `bbctest/gc/lambdas` se prueba lo siguiente:

- Se testea que un programa se falle al declarar una lambda de tamaño mayor al declarado para el heap sin usar GC.
- Se testea que un programa de una lambda, que no sobrepase el límite de tamaño del heap, funcione correctamente sin usar GC. 
- Se testea que un programa, al utilizar múltiples lambdas que suman un tamaño mayor al declarado del heap, no falle dado que usa GC.
- Se testea que un programa, al utilizar múltiples lambdas que suman un tamaño mayor al declarado del heap, falle dado que **NO** usa GC.