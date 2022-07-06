pub struct Board {
    pub bitboard: u64,
    // How should this be implemented
    pub queue: u32,
    pub hold: u8,
}

pub struct Move {
    // idk
    // x, y, mino, ishold, rotation
    // 0 <= x < 10
    // 0 <= y < 4
    // mino is one of 7 possible values
    // ishold is a bool
    // rotation is one of 4 values
}

impl Board {
    pub fn make_move(&mut self, mv: &Move) {
        todo!();
    }
    // Undo redo is probably faster than copying a board every time
    pub fn undo_move(&mut self, mv: &Move) {
        todo!();
    }
    // Possibly only want to incrementally generate moves to save on computation
    pub fn gen_moves(&self) -> Vec<Move> {
        todo!();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
