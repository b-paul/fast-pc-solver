use crate::four_line_board::FourLineBoard;
use crate::types::*;

pub struct Board {
    pub grid: [[CellColour; 10]; 40],
    pub queue: Vec<Mino>,
    pub hold: Option<Mino>,
    pub piece: Mino,
}

#[derive(Debug)]
pub enum SolverError {
    /// The supplied board had a cell above the maximum height allowed by a solver
    AboveMaxLineHeight,
}

impl Board {
    pub fn to_four_line(&self) -> Result<FourLineBoard, SolverError> {
        for y in 4..40 {
            for cell in self.grid[y] {
                if cell != CellColour::EMPTY {
                    return Err(SolverError::AboveMaxLineHeight);
                }
            }
        }

        let mut board = 0u64;

        for y in 0..4 {
            for x in 0..10 {
                if self.grid[y][x] != CellColour::EMPTY {
                    board |= 1u64 << (10 * y + x);
                }
            }
        }

        let mut queue = u32::MAX;

        for piece in (&self.queue).iter().rev() {
            queue <<= 3;
            queue |= match piece {
                Mino::I => 0,
                Mino::T => 1,
                Mino::O => 2,
                Mino::J => 3,
                Mino::L => 4,
                Mino::S => 5,
                Mino::Z => 6,
            }
        }

        Ok(FourLineBoard {
            board,
            hold: self.hold,
            piece: Some(self.piece),
            queue,
            cleared: 0,
        })
    }
}
