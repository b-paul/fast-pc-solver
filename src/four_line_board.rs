use crate::solver::{Board, Move};

pub struct FourLineBoard {
    pub bitboard: u64,
    // How should this be implemented
    pub queue: u32,
    pub hold: u8,
}

pub struct FourLineMove {
    // x, y, mino, hold, rotation
    // 0 <= x < 10
    // 0 <= y < 4
    // mino is one of 7 possible values
    // hold is a bool
    // rotation is one of 4 values
}

impl Move for FourLineMove {}
impl Board<FourLineMove> for FourLineBoard {
    fn make_move(&mut self, mv: &FourLineMove) {
        todo!();
    }
    // Undo redo is probably faster than copying a board every time
    fn undo_move(&mut self, mv: &FourLineMove) {
        todo!();
    }
    // Possibly only want to incrementally generate moves to save on computation
    fn gen_moves(&self) -> Vec<FourLineMove> {
        todo!();
    }

    fn solved(&self) -> bool {
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
