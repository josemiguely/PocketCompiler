# Entrega 4

## Integrantes: Stevens Egli, José Miguel Yuseff

---

## Implementación de un GC 

Se uso el algoritmo de Cheney's. Para esto, se agrego al compilador una llamada a la función de c `try_gc`.

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

- Se 
- Se 
