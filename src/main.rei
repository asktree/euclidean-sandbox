open Primitives;


type ghost = Ghost(primitive);
type ghostWorld = list(ghost);


let append_ghost: ghost => ghostWorld => ghostWorld;

let check_duplicate: ghost => ghostWorld => bool;

/* let nearest_ghost: point => ghostWorld => option((float, point, ghost)); */

let snap_cursor: point => float => ghostWorld => point;
