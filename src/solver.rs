use crate::four_line_board::*;

impl Board {
    fn solution(&mut self) -> Option<Vec<Move>> {
        if self.bitboard == 0 {
            return Some(vec![]);
        }

        // TODO pruning

        let moves = self.gen_moves();

        for mv in moves {
            self.make_move(&mv);
            if let Some(mut sol) = self.solution() {
                self.undo_move(&mv);

                let mut vec = vec![mv];

                vec.append(&mut sol);

                return Some(vec);
            }
            self.undo_move(&mv);
        }
        None
    }
    
    fn solutions(&mut self) -> Option<Vec<Vec<Move>>> {
        todo!();
    }
}
