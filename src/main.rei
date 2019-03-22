open Primitives;


type ghost = Ghost(primitive);
type ghostWorld = list(ghost);


let append_ghost: primitive => ghostWorld => ghostWorld;

let check_duplicates: ghost => ghostWorld => bool;

let nearest_ghost: point => ghostWorld => (float, point, ghost);

let snap_cursor: point => ghostWorld => point;
