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


let rec ghosts_within = (~qualified = ?, pt : point, r : float, w : ghostWorld) => {
    switch w {
        | [] => qualified
        | [g, ...remaining] => {
            let (dst, pt) = Euclidean.nearest_point_on_primitive(pt, g);

            if (dst <= r) {
                ghosts_within(~qualified = [g, ...qualified], pt, r, remaining)
            } else {
                ghosts_within(~qualified = qualified, pt, r, remaining)
            }
        }
    };
};

let snap_cursor = (pt : point, r : float, w : ghostWorld) => {
    switch ghosts_within(pt, r, w) {
        | None => pt 
        | Some(primitives) => {
            // NOT IMPLEMENTED
            // I was trying to think of a functional way to do this
            // but I think I should just write an imperative for loop
            //type 
            // let overlaps = fold_left(())

            // let rec overlaps = > (~so_far = ?, to_test) => {
            //     switch to_test {
            //         | [] => so_far
            //         | [p, ...remaining] => {
            //             map(remaining)
            //             overlaps(~so_far = Euclidean.find_intersections(p, q) @ so_far, 
            //         }
            //     }
            }
        } 
    }
};