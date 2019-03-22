let EPSILON = 0.01;
let SQRT_EPSILON = sqrt(EPSILON);

let sqr = (a) => a * a;

type point = (float, float);
type svg = string;

type line = (point, point);
type circle = (point, float);

type primitive = Line(line) | Circle(circle) | Point(point);
type ghost = Ghost(primitive);
type ghostWorld = list(ghost);

exception Not_implemented;

let is_identical = (pr1 : primitive, pr2 : primitive) => raise(Not_implemented);

let circle_circle_intersections = (c1 : circle, c2 : circle) => {
    let (center1, r1) = c1;
    let (center2, r2) = c2;

    let centerDistance = {
        let dot = (a, b) => fst(a) * fst(b) + snd(a) + snd(b);
        let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
        sqrt(dot(center2 - center1, center2 - center1))
    };

    if (centerDistance > r1 + r2) {
        []
    } else if (centerDistance == r1 + r2) {
        let (+) = (a, b) => (fst(a) + fst(b), snd(a) + snd(b));
        let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
        let (*) = (a, b) => (a * fst(b), a * snd(b));
        let (/) = (a, b) => (fst(b) / a, snd(b) / a); 

        [(r1 * (center2 - center1) / centerDistance) + center1]
    } else {
        let para = (sqr(centerDistance) - sqr(r2) + sqr(r1))/(2 * centerDistance);
        let perp = sqrt(
            (-centerDistance + r2 - r1) * 
            (-centerDistance - r2 + r1) *
            (-centerDistance + r2 + r1) *
            (centerDistance + r2 + r1)
            )/centerDistance;

        let (+) = (a, b) => (fst(a) + fst(b), snd(a) + snd(b));
        let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
        let (*) = (a, b) => (a * fst(b), a * snd(b));
        let (/) = (a, b) => (fst(b) / a, snd(b) / a); 

        let paraVec = (center2 - center1) / centerDistance;
        let perpVec = (-fst(paraVec), snd(paraVec));

        [para * paraVec + perp * perpVec + center1, para * paraVec - perp * perpVec + center1]
    }
};

let circle_line_intersections = (c : circle, l : line) => {
    let (center, radius) = c;
    let center = center - fst(l);
    let s = snd(l) - fst(l);

    let dot = (a, b) => fst(a) * fst(b) + snd(a) + snd(b);
    let magnitude = (x) => sqrt(dot(x, x)); 

    let paraVec = {
        let (*) = (a, b) => (a * fst(b), a * snd(b));

        ((dot(center, s) / dot(s, s)) * s) - center
    };

    let para = magnitude(paraVec);

    if (sqr(r) > sqr(para)) {
        let perp = sqrt(sqr(r) - sqr(para));

        let perpVec = (-fst(paraVec), snd(paraVec));

        [paraVec + perp * perpVec + fst(l), paraVec - perp * perpVec + fst(l)]
    } else if (sqr(r) == sqr(para)) {
        [paraVec + fst(l)]
    } else {
        []
    }
};

let circle_point_intersections = (c : circle, p : point) => {
    let (center, radius) = c;
    let dot = (a, b) => fst(a) * fst(b) + snd(a) + snd(b);
    let diff = {
        let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
        p - center
    };
    
    if (abs(sqrt(dot(diff, diff)) - radius) > EPSILON) {
        []
    } else {
        [p]
    }
};

let line_line_intersections = (l1 : line, l2 : line) => {
    let (dv, dx) = {
        let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
        let vx = snd(l1) - fst(l1);
        let vy = snd(l2) - fst(l2);

        (vy - vx, fst(l1) - fst(l2))
    };

    let factor = fst(dx) / fst(dv);

    if (factor * snd(dv) - snd(dx) > EPSILON) {
        []
    } else {
        let (+) = (a, b) => (fst(a) + fst(b), snd(a) + snd(b));
        let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
        let (*) = (a, b) => (a * fst(b), a * snd(b));

        [fst(l1) + factor * (snd(l1) - fst(l1))]
    }
};

let line_point_intersections = (l : line, p: point) => {
    let (+) = (a, b) => (fst(a) + fst(b), snd(a) + snd(b));
    let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
    let (*) = (a, b) => (a * fst(b), a * snd(b));
    let (/) = (a, b) => (fst(b) / a, snd(b) / a);
    let dot = (a, b) => fst(a) * fst(b) + snd(a) + snd(b);

    let v = snd(l) - fst(l);
    let p = p - fst(l);
    let proj = (dot(v, p)/dot(p, p)) * p;

    if (dot(v - proj, v - proj) > SQRT_EPSILON) {
        []
    } else {
        [p + fst(l)]
    } 
};

let find_intersections = (pr1, pr2) => 
    switch pr1 {
    | Circle(c1) => 
        switch pr2 {
        | Circle(c2) => circle_circle_intersections(c1, c2)
        | Line(l2) => circle_line_intersections(c1, l2)
        | Point(p2) => circle_point_intersections(c1, p2)
        }
    | Line(l1) =>
        switch pr2 {
        | Circle(c2) => circle_line_intersections(c2, l1)
        | Line(l2) => line_line_intersections(l1, l2)
        | Point(p2) => line_point_intersections(l1, p2)
        }
    | Point(p1) => 
        switch pr2 {
        | Circle(c2) => circle_point_intersections(c2, p1)
        | Line(l2) => line_point_intersections(l2, p1)
        | Point(p2) => is_identical(Point(p1), Point(p2))
        }
    };

let nearest_point_on_circle = (pt: point, c: circle) => {
    let (+) = (a, b) => (fst(a) + fst(b), snd(a) + snd(b));
    let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
    let (*) = (a, b) => (a * fst(b), a * snd(b));
    let (/) = (a, b) => (fst(b) / a, snd(b) / a);
    let dot = (a, b) => fst(a) * fst(b) + snd(a) + snd(b);

    let vec = pt - fst(c);
    let vec = snd(c) * vec / sqrt(dot(vec, vec));
    return vec + fst(c);
};

let nearest_point_on_line = (pt: point, l: line) => {
    let (+) = (a, b) => (fst(a) + fst(b), snd(a) + snd(b));
    let (-) = (a, b) => (fst(a) - fst(b), snd(a) - snd(b));
    let (*) = (a, b) => (a * fst(b), a * snd(b));
    let (/) = (a, b) => (fst(b) / a, snd(b) / a);
    let dot = (a, b) => fst(a) * fst(b) + snd(a) + snd(b);

    let vec = snd(line) - fst(line);
    let vec = vec / sqrt(dot(vec, vec));
    let dst = dot(vec, pt);

    return (dst * vec) + fst(line); 
};

let nearest_point_on_primitive = (pt, pr) =>
    switch pr {
    | Circle(c) => ()
    | Line(l) => ()
    | Point(p) => (distance(pt, p), p)
    };

let rec nearest_ghost = (~best = ?, pt : point, w : ghostWorld) =>
    switch w {
    | [] => best
    | [g, ...remaining] => 
        let Ghost(pr) = g;
        let (d1, x1) = nearest_point_on_primitive(pt, pr);
        let candidate = (d1, x1, g);
        switch best {
        | None => nearest_ghost(~best = candidate, pt, remaining)
        | Some(incumbent) => 
            let winner = max(candidate, incumbent);
            nearest_ghost(~best = winner, pt, remaining)
        };
    };