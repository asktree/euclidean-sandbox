type point = (float, float);
type svg = string;

type line = (point, point);
type circle = (point, float);

type primitive = Line(line) | Circle(circle) | Point(point);
type ghost = Ghost(primitive);
type ghostWorld = list(ghost);

let draw: ghost => svg;

let find_intersections: primitive => primitive => list(point);

let append_ghost: primitive => ghostWorld => ghostWorld;

let check_duplicates: ghost => ghostWorld => bool;

let nearest_point_on_primitive: point => primitive => (float, point);

let nearest_ghost: point => ghostWorld => (float, point, ghost);

let snap_cursor: point => ghostWorld => point;
