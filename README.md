![green-divider](https://user-images.githubusercontent.com/7065401/52071924-c003ad80-2562-11e9-8297-1c6595f8a7ff.png)
### Instalar Git (Guia absolutamente minimal de git)
Acontinuacion usando la terminal
### Configuracion (una sola vez)

```git config --global user.name “[firstname lastname]”```<br />
establece tu nombre de usuario en github

```git config --global user.email “[valid-email]”```<br />
establece tu email asociado

### Cada sesion

#### Abrir la terminal en la carpeta que va a trabajar
```git clone https://github.com/aaronbernal28/DevDynasty.git```<br />
clona todo el repositorio en una carpeta

#### Abril esa carpeta en la terminal
```cd devDynasty``` (PS C:\Users\**\Desktop\devDynasty> en windows por ejemplo) en adelante los siguientes comandos se deben usar en esa direccion porque ahi esta la copia del repositorio

```git branch```<br />
enumere todas las ramas (incluida la principal). aparecerá un * junto a la rama actualmente activa

```git branch [branch-name]``` <br />
crear una nueva rama en la confirmación actual (no trabajar directamente sombre la rama principal mean)

#### Para trabajar con esa nueva rama
```git checkout [branch-name]``` <br />
cambie a otra rama para que sea la activa, puede comprobrar que efectivamente cambio con ```git branch```

```git push -u origin [branch-name]```<br />
para subir esta nueva rama al repositorio en github (si no solo va a esta almacenado en su dispositivo)

#### Cuando termina de trabajar (en vs code)

```git add iap1-tp.hs```<br />
debe preparar los cambios antes de confirmarlos (en general vamos a editar solo ese archivo 'iap1-tp.hs')

```git commit -m "[comentario]"```<br />
esto creará una nueva confirmación con los cambios que realizó, junto con un mensaje de confirmación que describe los cambios

```git push``` <br />
es necesario después de confirmar para enviar los cambios al repositorio remoto en GitHub.

#### Solicitar que su trabajo se implemente en la rama principal
Para ello use la pagina de github que es mas amigable, tiene que crear un ```New pull request```, seleccionar la rama con la que trabajo y que sea implementar en el ```mean```.

### Consejos
Esta un forma rapida que encontre para comenzar a trabajar en el TP, seguro que van a encontrar mas optimas. Recuerden que no es buena practica trabajar directamente con la rama principal para eso tienen que crearse sus ramas. Adjunto algunos consejos de la teorica 4
- Hacer commits peque˜nos y puntuales, con la mayor frecuencia posible.
- Mantener actualizada la copia local del repositorio, para estar sincronizados con el resto del equipo.
- Commitear los archivos fuente, nunca los archivos derivados! (no se que significa eso xd)
- Manejar inmediatamente los conflictos. (Esto es cuando crean New pull requests)

![green-divider](https://user-images.githubusercontent.com/7065401/52071924-c003ad80-2562-11e9-8297-1c6595f8a7ff.png)
