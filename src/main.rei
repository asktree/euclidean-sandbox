open Primitives;

type svg = string;
type ghost = Ghost(primitive);
type ghostWorld = list(ghost);

let draw: ghost => svg;



let append_ghost: primitive => ghostWorld => ghostWorld;

let check_duplicates: ghost => ghostWorld => bool;

let nearest_point_on_primitive: point => primitive => (float, point);

let nearest_ghost: point => ghostWorld => (float, point, ghost);

let snap_cursor: point => ghostWorld => point;
