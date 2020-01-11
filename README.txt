PoP Ugeopgave 12g - Mila (brx290) og Christian(wjx230)

= Kode =
Koden ligger i src mappen. De kan oversættes med fsharpc og køres med mono.
1. Kompiler  chess og pieces bibliotekerne:
   fsharpc -a chess.fs 
   fsharpc -r chess.dll -a pieces.fs
2. Kompiler programmet og link med chess og pieces bibliotekerne:
   fsharpc -r chess.dll -r pieces.dll PROGRAMNAME.fsx
   hvor `PROGRAMNAME` er navnet af din .fsx fil. (f.eks. chessApp.fsx)
3. Kør programmet med mono:
   mono PROGRAMNAME.exe 