use crate::types::*;
use rustc_hash::FxHashSet;

/// Board is represented using a bitboard
/// Based on wirelyre's idea
#[derive(Copy, Clone)]
pub struct FourLineBoard {
    /// The first 40 bits of this value are used to represent a 10x4 grid of cells.
    pub(crate) board: u64,
    pub(crate) hold: Option<Mino>,
    pub(crate) piece: Option<Mino>,
    /// Bit array thingo
    /// Shift right by 3 and bitwise AND by 7 to get the next piece
    pub(crate) queue: u32,
    /// Tracker for how many lines have been cleared so far
    pub(crate) cleared: u8,
}

impl From<u8> for Mino {
    fn from(n: u8) -> Mino {
        [
            Mino::I,
            Mino::T,
            Mino::O,
            Mino::J,
            Mino::L,
            Mino::S,
            Mino::Z,
        ][n as usize]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FourLineMove {
    did_hold: bool,
    piece: Mino,
    rotation: Rotation,
    x: u8,
    y: u8,
}

impl FourLineMove {
    /// Piece mask, min x, min y, max x
    fn piece_mask(&self) -> (u64, u8, u8, u8) {
        match self.piece {
            Mino::I => match self.rotation {
                Rotation::North => (0xF, 1, 0, 7),
                Rotation::East => (0x40100401, 0, 2, 9),
                Rotation::South => (0xF, 2, 0, 8),
                Rotation::West => (0x40100401, 0, 1, 9),
            },
            Mino::T => match self.rotation {
                Rotation::North => (0x807, 1, 0, 8),
                Rotation::East => (0x100C01, 0, 1, 8),
                Rotation::South => (0x1C02, 1, 1, 8),
                Rotation::West => (0x200C02, 1, 1, 9),
            },
            Mino::O => match self.rotation {
                Rotation::North => (0xC03, 0, 0, 8),
                Rotation::East => (0xC03, 0, 1, 8),
                Rotation::South => (0xC03, 1, 1, 9),
                Rotation::West => (0xC03, 1, 0, 9),
            },
            Mino::J => match self.rotation {
                Rotation::North => (0x407, 1, 0, 8),
                Rotation::East => (0x300401, 0, 1, 8),
                Rotation::South => (0x1C04, 1, 1, 8),
                Rotation::West => (0x200803, 1, 1, 9),
            },
            Mino::L => match self.rotation {
                Rotation::North => (0x1007, 1, 0, 8),
                Rotation::East => (0x100403, 0, 1, 8),
                Rotation::South => (0x1C01, 1, 1, 8),
                Rotation::West => (0x300802, 1, 1, 9),
            },
            Mino::S => match self.rotation {
                Rotation::North => (0x1803, 1, 0, 8),
                Rotation::East => (0x100C02, 0, 1, 8),
                Rotation::South => (0x1803, 1, 1, 8),
                Rotation::West => (0x100C02, 1, 1, 9),
            },
            Mino::Z => match self.rotation {
                Rotation::North => (0xC06, 1, 0, 8),
                Rotation::East => (0x200C01, 0, 1, 8),
                Rotation::South => (0xC06, 1, 1, 8),
                Rotation::West => (0x200C01, 1, 1, 9),
            },
        }
    }

    fn is_four_line(&self, board: &FourLineBoard) -> bool {
        let piece_height = match self.piece {
            Mino::I => match self.rotation {
                Rotation::North => 0,
                Rotation::East => 1,
                Rotation::South => 0,
                Rotation::West => 2,
            },
            Mino::O => match self.rotation {
                Rotation::North => 1,
                Rotation::East => 0,
                Rotation::South => 0,
                Rotation::West => 1,
            },
            _ => match self.rotation {
                Rotation::North => 1,
                Rotation::East => 1,
                Rotation::South => 0,
                Rotation::West => 1,
            },
        };

        self.y + piece_height < 4 - board.cleared
    }

    fn collision(&self, board: &FourLineBoard) -> bool {
        let (mut mask, min_x, min_y, max_x) = self.piece_mask();

        if self.x > max_x || self.x < min_x || self.y < min_y || self.y > 10 {
            return true;
        }

        if self.y > 6 {
            return false;
        }

        mask <<= (self.x - min_x + 10 * (self.y - min_y)) % 64;

        (mask & board.board) != 0
    }

    fn shift(&self, board: &FourLineBoard, shift: Shift) -> Option<Self> {
        let shift = match shift {
            Shift::TapRight => 1,
            Shift::TapLeft => -1,
        };
        let x = ((self.x as i8) + shift) as u8;

        let mv = FourLineMove { x, ..*self };

        if mv.collision(board) {
            return None;
        }
        Some(mv)
    }

    fn rotate(&self, board: &FourLineBoard, rotation: Spin) -> Option<Self> {
        let rot = self.rotation.apply(&rotation);

        let srs_table = self.piece.srs_table(self.rotation, rotation);

        for srs_entry in srs_table {
            let mv = FourLineMove {
                rotation: rot,
                x: ((self.x as i8) - srs_entry.0) as u8,
                y: ((self.y as i8) + srs_entry.1) as u8,
                ..*self
            };
            if !mv.collision(board) {
                return Some(mv);
            }
        }

        None
    }
}

impl FourLineBoard {
    // TODO Fix this lol
    fn hold(&self) -> Option<Mino> {
        if self.hold == None {
            return match self.queue & 7 {
                0 => Some(Mino::I),
                1 => Some(Mino::T),
                2 => Some(Mino::O),
                3 => Some(Mino::J),
                4 => Some(Mino::L),
                5 => Some(Mino::S),
                6 => Some(Mino::Z),
                _ => None,
            };
        }
        self.hold
    }

    fn next_piece(&self) -> Option<Mino> {
        match self.queue & 7 {
            0 => Some(Mino::I),
            1 => Some(Mino::T),
            2 => Some(Mino::O),
            3 => Some(Mino::J),
            4 => Some(Mino::L),
            5 => Some(Mino::S),
            6 => Some(Mino::Z),
            _ => None,
        }
    }

    fn next_next_piece(&self) -> Option<Mino> {
        match (self.queue >> 3) & 7 {
            0 => Some(Mino::I),
            1 => Some(Mino::T),
            2 => Some(Mino::O),
            3 => Some(Mino::J),
            4 => Some(Mino::L),
            5 => Some(Mino::S),
            6 => Some(Mino::Z),
            _ => None,
        }
    }

    fn drop_y(&self, mv: FourLineMove) -> u8 {
        let (mut mask, min_x, min_y, _) = mv.piece_mask();

        mask <<= mv.x - min_x;

        for y in (min_y..mv.y).rev() {
            let mask: u64 = mask << ((10 * (y - min_y)) % 64);
            if mask & self.board != 0 {
                return y + 1;
            }
        }

        min_y
    }
}

impl FourLineBoard {
    fn make_move(&mut self, mv: &FourLineMove) -> FourLineBoard {
        let (piece_mask, min_x, min_y, _) = mv.piece_mask();

        let mut board = self.board | piece_mask << (10 * (mv.y - min_y) + (mv.x - min_x));
        let mut cleared = self.cleared;

        // Rev so that you don't miss a line if you clear multiple lines
        // e.g. you clear line 1 then line 2 shifts down to now be line 1, but without reversing
        // you look at the new line 2, not the old line 2, but reversing mitigates this
        for i in (0..(4 - self.cleared)).rev() {
            let mask = 0x3FF << (10 * i);
            if board & mask == mask {
                board = unsafe { core::arch::x86_64::_pext_u64(board, !mask) };
                cleared += 1;
            }
        }

        let hold = if mv.did_hold { self.piece } else { self.hold };

        let piece = if mv.did_hold && self.hold == None {
            self.next_next_piece()
        } else {
            self.next_piece()
        };

        let queue = if mv.did_hold && self.hold == None {
            self.queue >> 6
        } else {
            self.queue >> 3
        };

        FourLineBoard {
            board,
            cleared,
            hold,
            queue,
            piece,
        }
    }

    fn solved(&self) -> bool {
        debug_assert!(self.cleared <= 4);

        self.cleared == 4
    }
}

struct FourLineMoveGenerator {
    board: FourLineBoard,
    stack: Vec<FourLineMove>,
    table: FxHashSet<FourLineMove>,
    hd_table: FxHashSet<u64>,
}

impl FourLineMoveGenerator {
    fn new(board: FourLineBoard) -> Self {
        let mut stack = Vec::new();

        let mut table = FxHashSet::default();
        let hd_table = FxHashSet::default();

        // get the first move!
        if let Some(piece) = board.piece {
            let mv = FourLineMove {
                did_hold: false,
                piece,
                rotation: Rotation::North,
                x: 5,
                y: 4,
            };
            stack.push(mv);
            table.insert(mv);
        }

        if let Some(piece) = board.hold() {
            let mv = FourLineMove {
                did_hold: true,
                piece,
                rotation: Rotation::North,
                x: 5,
                y: 4,
            };
            stack.push(mv);
            table.insert(mv);
        }

        FourLineMoveGenerator {
            board,
            stack,
            table,
            hd_table,
        }
    }

    fn next(&mut self) -> Option<FourLineMove> {
        while let Some(mv) = self.stack.pop() {
            // do stuff
            // Add all of the operations that stem off of this action to the stack
            for shift in [Shift::TapLeft, Shift::TapRight] {
                if let Some(mv) = mv.shift(&self.board, shift) {
                    if !self.table.contains(&mv) {
                        self.stack.push(mv);
                        self.table.insert(mv);
                    }
                }
            }
            for spin in [Spin::Clockwise, Spin::AntiClockwise, Spin::Half] {
                if let Some(mv) = mv.rotate(&self.board, spin) {
                    if !self.table.contains(&mv) {
                        self.stack.push(mv);
                        self.table.insert(mv);
                    }
                }
            }
            let drop = FourLineMove {
                y: self.board.drop_y(mv),
                ..mv
            };

            // Add soft drop
            if !self.table.contains(&drop) {
                self.stack.push(drop);
                self.table.insert(drop);
            }

            let (piece_mask, min_x, min_y, _) = drop.piece_mask();

            let piece_mask = piece_mask << ((10 * (drop.y - min_y) + drop.x - min_x) % 64);

            if drop.is_four_line(&self.board) && !self.hd_table.contains(&piece_mask) {
                self.hd_table.insert(piece_mask);
                return Some(drop);
            }
        }
        None
    }
}

impl FourLineBoard {
    pub fn solution(&mut self) -> Option<Vec<FourLineMove>> {
        if self.solved() {
            return Some(vec![]);
        }

        // TODO pruning

        let mut move_generator = FourLineMoveGenerator::new(*self);

        while let Some(mv) = move_generator.next() {
            let mut new_board = self.make_move(&mv);
            if let Some(mut sol) = new_board.solution() {
                let mut vec = vec![mv];

                vec.append(&mut sol);

                return Some(vec);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interface_board::*;
    use crate::types::CellColour::*;
    use crate::types::Mino::*;
    #[test]
    fn tetris() {
        // Queue I
        let mut board = Board {
            grid: [[CellColour::EMPTY; 10]; 40],
            hold: None,
            piece: I,
            queue: vec![],
        };
        board.grid[0] = [CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY];
        board.grid[1] = [CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY];
        board.grid[2] = [CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY];
        board.grid[3] = [CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY];

        let mut four_line = board.to_four_line().unwrap();

        let solution = four_line.solution();

        println!("Moves are: {:?}", solution);

        assert!(solution.is_some());
    }
    #[test]
    fn random_jaws() {
        // Queue OTJJOL
        let mut board = Board {
            grid: [[CellColour::EMPTY; 10]; 40],
            hold: None,
            piece: O,
            queue: vec![J, T, S, Z, L],
        };
        board.grid[3] = [
            CYAN, ORANGE, ORANGE, ORANGE, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
        ];
        board.grid[2] = [
            CYAN, ORANGE, RED, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
        ];
        board.grid[1] = [
            CYAN, RED, RED, GREEN, GREEN, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
        ];
        board.grid[0] = [
            CYAN, RED, GREEN, GREEN, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
        ];

        let mut four_line = board.to_four_line().unwrap();

        let solution = four_line.solution();

        println!("Moves are: {:?}", solution);

        assert!(solution.is_some());
    }
}
