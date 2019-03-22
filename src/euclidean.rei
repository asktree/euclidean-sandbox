type point = (float, float);
type line = (point, point);
type circle = (point, float);

let circle_circle_intersections: circle => circle => list(point);
