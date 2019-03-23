open Main;

type spoint = (string, string);
type sline = (spoint, spoint);
type scircle = (spoint, string);

type stringy_primitive = StringLine(sline) | StringCircle(scircle) | StringPoint(spoint);

let stringify_primitive = (pr:Primitives.primitive) => {
    let sof = Js.Float.toString;    
    switch (pr) {
    | Primitives.Circle((pt, r)) => StringCircle(((sof(fst(pt)), sof(snd(pt))), sof(r)))
    | Primitives.Line((p1, p2)) => StringLine(((sof(fst(p1)), sof(snd(p1))), (sof(fst(p2)), sof(snd(p2)))))
    | Primitives.Point(pt) => StringPoint((sof(fst(pt)), sof(snd(pt))))
    };
};

let draw_ghost = (Ghost(pr)) => {
    let spr = stringify_primitive(pr);
    switch (spr) {
    | StringCircle((pt, r)) => 
        <circle cx={fst(pt)} cy={snd(pt)} r={r} stroke="black" fill="none"/>
    | StringLine((p1, p2)) => 
        <line x1={fst(p1)} y1={snd(p1)} x2={fst(p2)} y2={snd(p2)} stroke="black"/>
    | StringPoint(pt) => 
        <circle cx={fst(pt)} cy={snd(pt)} r="2" fill="black"/>
    };
};

let rec draw_world = (~elems:list(ReasonReact.reactElement)=[], w) => {
    switch (w) {
    | [] => 
        let children = Array.of_list(elems);
        <g> ...children </g>
    | [h, ...k] => 
        let next_elem = draw_ghost(h);
        let now_drawn = [next_elem, ...elems];
        draw_world(~elems=now_drawn, k)
    };
};

type tool = CircleTool | LineTool | PointTool;
type state = {
    ghosts: ghostWorld,
    selected_tool: tool,
    tool_firstclick: option(Primitives.point),
    hovered_ghost: option(ghost),
    tool_pos: Primitives.point,
};

type action =
  | ToolSelect(tool)
  | ClickCanvas
  | MouseMove((int, int));

  
let component = ReasonReact.reducerComponent("Canvas");



let make = (_children) => {
    let canvas_mousemove = (e, self) => {
        let dim = [%bs.raw {| 'e.getBoundingClientRect();' |}]
        Js.Console.log(dim);
        let pt = (ReactEvent.Mouse.clientX(e), ReactEvent.Mouse.clientY(e));
        self.ReasonReact.send(MouseMove(pt));
        ()
    };
    {
        ...component,

        initialState: () => {
            ghosts: [],
            selected_tool: CircleTool,
            tool_firstclick: None,
            hovered_ghost: None,
            tool_pos: (0., 0.),
        },

        reducer: (action, state) => {
            switch (action) {
            | ToolSelect(t) => 
                ReasonReact.Update({...state, selected_tool: t, tool_firstclick: None})
            | ClickCanvas => 
                let pt = state.tool_pos;
                let pt' = snap_cursor(pt, 20., state.ghosts);
                switch (state.selected_tool) {
                | PointTool =>                     
                    let new_ghost = Ghost(Primitives.Point(pt'))
                    let new_ghosts = append_ghost(new_ghost, state.ghosts);
                    ReasonReact.Update({...state, tool_firstclick: None, ghosts: new_ghosts})
                | CircleTool =>
                    switch (state.tool_firstclick) {
                    | None => 
                        ReasonReact.Update({...state, tool_firstclick: Some(pt')})
                    | Some(pt2) => 
                        let r = Euclidean.distance(pt', pt2)
                        let new_ghost = Ghost(Primitives.Circle((pt2, r)))
                        let new_ghosts = append_ghost(new_ghost, state.ghosts);
                        ReasonReact.Update({...state, tool_firstclick: None, ghosts: new_ghosts})
                    }
                | LineTool =>
                    switch (state.tool_firstclick) {
                        | None => 
                            ReasonReact.Update({...state, tool_firstclick: Some(pt')})
                        | Some(pt2) => 
                            let new_ghost = Ghost(Primitives.Line((pt2, pt')))
                            let new_ghosts = append_ghost(new_ghost, state.ghosts);
                            ReasonReact.Update({...state, tool_firstclick: None, ghosts: new_ghosts})
                        }
                }
            | MouseMove(ipt) => 
                let pt = (float_of_int(fst(ipt)), float_of_int(snd(ipt)));
                let pt' = snap_cursor(pt, 10., state.ghosts);
                ReasonReact.Update({...state, tool_pos: pt'})
            }
        },

        render: self => {
            <div>
                <svg width="1000" height="1000" 
                 onClick=(_event => self.send(ClickCanvas))
                 onMouseMove={self.handle(canvas_mousemove)}>
                    <circle 
                        cx={Js.Float.toString(fst(self.state.tool_pos))}
                        cy={Js.Float.toString(snd(self.state.tool_pos))}
                        r="3" fill="red" stroke="red"
                    />
                    {draw_world(self.state.ghosts)}
                </svg>
            </div>
        }
    };
};
