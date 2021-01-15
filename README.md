<style>

  h1,h2,h3 {
    color:#619776;
  }

</style>

<img src = 'Multimedia/Imagenes/Marco Analítico H.png'>

<h1 style = 'text-align:center;'>Proyecto de metodología, diagnóstico y propuesta de cuidados a nivel municipal</h1>

<p style = 'color:gray; text-align:center;'>Laboratorio Nacional de Políticas Públicas (LNPP)<br>
Centro de Investigación y Docencia Económicas (CIDE)<br>
15 de enero de 2021</p>

## Contenido del repositorio:

Los scripts contenidos en este repositorio miden los indicadores de los atributos del derecho a cuidar y del derecho a ser cuidada o cuidado, de acuerdo con la metodología desarrollada en el documento _"Marco analítico y metodología para diagnosticar las brechas en los cuidados en municipios de México"_.

Para comprender lo que se mide en cada indicador es necesario antes revisar documento mencionado.

## Scripts:

Los scripts están agrupados siguiendo la lógica del documento, de tal manera que la carpeta **"Derecho a ser cuidadx - Cobertura"** contiene todos los scripts que miden cobertura de los distintos cuidados (personal, de desarrollo integral, de salud) para los tres grupos de interés (infancia temprana, personas con discapacidad, personas adultas mayores). Nótese que el Derecho a cuidar no se divide por tipo de cuidado, por lo que todos los scripts correspondientes a este derecho se alojan dentro de la única carpeta **"Derecho a cuidar"**.

El nombre de los scripts corresponde a la siguiente información: 
### Nombres de los scripts

$$Grupo de interés + _ + Tipo de cuidado + _ + Atributo del derecho + _ + Proveedor del servicio$$

Por ejemplo, `IT_CP_Cob_Hog.R` es el script que mide el _indicador de cobertura de cuidados personales para infancia temprana desde el hogar_, y así sucesivamente.

El único script que no cabe dentro de ésta lógica es *A1_EstimacionesCrecimientoPoblacional.r* que corresponde a la información del Anexo 1 del documento metodológico.

### Replicabilidad con otros municipios

Cada script está hecho de tal manera que el diagnóstico que se realizó para Manzanillo, Colima se puede recrear para cualquier municipio del país. 

Para ello, cada script tiene una sección llamada **"Seleccionar municipio y estado de interés"** (o, en sus variaciones, **"seleccionar municipio de interés"** o **"seleccionar estado de interés"** según la representatividad de las bases utilizadas para ese indicador) en la que se le pide a la persona usuaria revisar el código correspondiente al municipio y estado de interés (en una base del INEGI) y cambiar el código en los objetos señalados. Después de este cambio, el script debe correr sin contratiempos para la localidad seleccionada.

### Versión de R utilizada

Todos los scripts se corren en la versión 4.0.3 de R, desde RStudio.

### Sobre el sistema operativo

Todos los scripts se pueden correr desde Mac o PC.

### Dudas y comentarios:

Si usted tiene cualquier duda o encuentra algún inconveniente al correr los scripts, por favor comuníquese con 
Natalia Torres en el correo nataliato94@gmail.com





