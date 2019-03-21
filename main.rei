type point = (float, float);
type svg = string;

type line = (point, point);
type circle = (point, float);

type primitive = line | circle | point;
type ghost = Ghost primitive;
type ghostWorld = list(ghost);

let draw: ghost => svg;

let find_intersections: ghost => ghost => list(point);

let append_ghost: primitive => ghostWorld => ghostWorld;

let check_duplicates: ghost => ghostWorld => bool;

let nearest_point: point => ghost => (point, float);
