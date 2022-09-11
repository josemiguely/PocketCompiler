# Entrega 1



## compile.ml

Se implementaron las siguientes operaciones y tipos:
1. Suma binaria sobre enteros (\+)
2. And lógico 
3. Comparador menor entre enteros (<)
4. Let binding
5. Booleanos



### <u> Suma Binaria (+) </u>

Para implementar operaciones binarias se utilizó la técnica del stack. De esta forma se pudo aplicar la suma a los argumentos de forma inmediata. Su implementación está en la función compile_expr.

### <u> And Lógico </u>

Se utilizó la técnica del stack para evaluar sus argumentos. Se aplicó también la semántica de atajos de evaluación. Esta semántica se implementó utilizando la instrucción assembler "*jmp*", en el que se salta a una nueva sección de código que entrega *false* en el caso de que el primer argumento sea falso. Su implementación está en la función compile_expr.


### <u> Comparador menor (<) </u>

Se utilizó la técnica del stack para evaluar sus argumentos. Su implementación está en la función compile_expr.


### <u> Let binding</u>

Se implemento el let como señalado en el apunte del curso.  Su implementación está en la función compile_expr.


### <u> Booleanos</u>

Los booleanos son representados como valores de 64 bits con el bit menos significativo en 1.El bit más significativo señala si es *true* (1) o *false* (0). El resto de los bits son 1's por convención. Su implementación está en la función compile_expr.


#### <u> Funciones auxiliares</u> 

- `asm_to_string`: Convierte una variable de tipo _instruction_ en string.
- `prim2_scaffold`: Realiza la estructura básica de toda operación binaria. Esta estructura se basa en la técnica del stack para obtener valores inmediatos. Para el caso de la operación lógica *and*, se realiza el atajo de evaluación dentro de esta función.

---

## asm.ml

Se añadieron las siguientes funcionalidades:

1. Nuevas Instrucciones: *IAdd*,*IMult*,*IAnd*,*ICmp*,*IJe*,*IJl*,*IJne*,*IJmp*,*IXor*,*ILabel*
2. Nuevos Registros: *RSP*,*R10*

---

## Testing

### **ADD1**
- Se testean la suma de un 1 a números positivos.
- Se testea la suma de 1 a números negativos.
- Se testea la suma de 1 varias veces consecutivas. 

### **SUB1**
- Se testean la posibilidad de restar 1 a números negativos.
- Se testean la posibilidad de restar 1 a números positivos.
- Se testean la posibilidad de restar 1 a cero.
- Se testea la resta de 1 varias veces consecutivas. 

### **ADD**

- Se testea la suma entre 2 números.
- Se testea la suma con varias operaciones.
  
### **Less than** 

- Se testean los casos en que hay un número mayor a otro.
- Se testean los casos en que hay un número menor a otro.
- Se testean la comparación de resultados en operaciones.
  
### **AND** 

- Se testea la operación entre un _true_ y _false_.
- Se testea la operación entre el mismo booleano.
- Se testea múltiples operaciones booleanas encadenadas.

### **NOT**

- Se testea la capacidad de negar un booleano _true_ y _false_.
- Se testea la capacidad de negar un booleano _false_.
- Se niegan varios _not_ consecutivos.

### **IF**

- Se testea la ejecución de la branch _true_.
- Se testea la ejecución de la branch _false_.
- Se testea un _if_ anidado, junto varias operaciones.

### **Let**

- Se testea un let simple de una variable.
- Se testean varios let anidados.
- Se testean un let con varias operaciones mezcladas.
