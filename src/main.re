type point = (float, float);
type svg = string;

type line = (point, point);
type circle = (point, float);

type primitive = Line(line) | Circle(circle) | Point(point);
type ghost = Ghost(primitive);
type ghostWorld = list(ghost);


let circle_circle_intersections = (c1 : circle, c2 : circle) => 
    raise(Not_implemented);

let circle_line_intersections = (c : circle, l : line) =>
    raise(Not_implemented);

let circle_point_intersections = (c : circle, p : point) =>
    raise(Not_implemented);
let line_line_intersections = (l1 : line, l2 : line) => 
    raise(Not_implemented);

let line_point_intersections = (l : line, p: point) =>
    raise(Not_implemented);

let is_identical = (pr1 : primitive, pr2 : primitive) => raise(Not_implemented);

let find_intersections = (Ghost(pr1) : ghost, Ghost(pr2) : ghost) => 
    switch pr1 {
    | (Circle(c1)) => 
        switch pr2 {
        | Circle(c2) => circle_circle_intersections(c1, c2)
        | Line(l2) => circle_line_intersections(c1, l2)
        | Point(p2) => circle_point_intersections(c1, p2)
        }
    | (Line(l1)) =>
        switch pr2 {
        | Circle(c2) => circle_line_intersections(c2, l1)
        | Line(l2) => line_line_intersections(l1, l2)
        | Point(p2) => line_point_intersections(l1, p2)
        }
    | (Point(p1)) => 
        switch pr2 {
            | Circle(c2) => circle_point_intersections(c2, p1)
            | Line(l2) => line_point_intersections(l2, p1)
            | Point(p2) => is_identical(Point(p1), Point(p2))
            }
    };
