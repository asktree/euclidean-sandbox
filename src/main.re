open Primitives;
open Euclidean;


type svg = string;

type ghost = Ghost(primitive);
type ghostWorld = list(ghost);

let rec check_duplicate = (g, w) => {
    switch (w) {
    | [] => false
    | [h, ...k] =>
        switch (Euclidean.is_identical(g, h)) {
        | true => true
        | false => check_duplicate(g, k)
        };
    };
};

let append_ghost = (g, w) => {
    [g, ...w]
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
