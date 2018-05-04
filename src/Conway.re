type cell =
  | Alive
  | Dead;

type state = array(array(cell));

let cellToInt = (grid, (i, j)) =>
  switch (grid[i][j]) {
  | Alive => 1
  | Dead => 0
  | exception (_) => 0
  };

let neighborsAlive = (i, j, grid) =>
  [
    (i, j + 1),
    (i, j - 1),
    (i - 1, j),
    (i + 1, j),
    (i + 1, j + 1),
    (i - 1, j + 1),
    (i - 1, j - 1),
    (i + 1, j - 1),
  ]
  |> List.map(cellToInt(grid))
  |> List.fold_left((acc, n) => acc + n, 0);

let evolve = grid => {
  let size = Array.length(grid);
  let newGrid = Array.make_matrix(size, size, Dead);

  for (i in 0 to size - 1) {
    for (j in 0 to size - 1) {
      newGrid[i][j] = (
        switch (grid[i][j], neighborsAlive(i, j, grid)) {
        | (Alive, 2)
        | (Alive, 3) => Alive
        | (Dead, 3) => Alive
        | _ => Dead
        }
      );
    };
  };
  newGrid;
};

let toggleCell = (i, j, grid) => {
  let newGrid = grid |> Array.copy;
  let col = newGrid[i] |> Array.copy;
  let cell = col[j];
  switch (cell) {
  | Alive =>
    col[j] = Dead;
    newGrid[i] = col;
  | Dead =>
    col[j] = Alive;
    newGrid[i] = col;
  };
  newGrid;
};

type action =
  | Toggle(int, int)
  | Evolve;

module Cell = {
  let make = (~state, ~onClick, _children) => {
    ...ReasonReact.statelessComponent("Cell"),
    render: _ => {
      <div 
        onClick=onClick
        style=ReactDOMRe.Style.make(
          ~width="50px", 
          ~height="50px", 
          ~border="1px solid #666", 
          ~backgroundColor=(switch (state) {
            | Alive => "#000"
            | Dead => "#fff"
            }), 
          ()
        )
      />
    }
  }
};

let make = (~size, _children) => {
  ...ReasonReact.reducerComponent("Conway"),
  initialState: () => Array.make_matrix(size, size, Dead),
  reducer: (action, state) =>
    switch (action) {
    | Toggle(i, j) => ReasonReact.Update(state |> toggleCell(i, j))
    | Evolve => ReasonReact.Update(evolve(state))
    },
  render: self =>
    <div>
      <button onClick={_ => self.send(Evolve)}>
        ("Evolve" |> ReasonReact.string)
      </button>
      (
        self.state
        |> Array.mapi((i, row) =>
             <div key=string_of_int(i) style=ReactDOMRe.Style.make(~display="flex", ())>
               (
                 row
                 |> Array.mapi((j, cellState) => 
                      <Cell key=string_of_int(j) state=cellState onClick=(_ => self.send(Toggle(i, j))) />
                    )
                 |> ReasonReact.array
               )
             </div>
           )
        |> ReasonReact.array
      )
    </div>,
};