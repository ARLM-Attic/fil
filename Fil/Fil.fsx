#load "Fil.fs"
open Fil

let f = compile <@ 2. + 2. > 3. @>
let x = f ()