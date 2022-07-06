pub trait Board<M: Move> {
    fn make_move(&mut self, mv: &M);
    fn undo_move(&mut self, mv: &M);
    fn gen_moves(&self) -> Vec<M>;
    fn solved(&self) -> bool;
}

pub trait Move {}

fn solution<M: Move, B: Board<M>>(board: &mut B) -> Option<Vec<M>> {
    if board.solved() {
        return Some(vec![]);
    }

    // TODO pruning

    let moves = board.gen_moves();

    for mv in moves {
        board.make_move(&mv);
        if let Some(mut sol) = solution(board) {
            board.undo_move(&mv);

            let mut vec = vec![mv];

            vec.append(&mut sol);

            return Some(vec);
        }
        board.undo_move(&mv);
    }
    None
}
