use crate::{consts::*, types::*};
use konst::array::from_fn;
use rustc_hash::FxHashSet;

use std::marker::PhantomData;

const COLUMN: u64 = 0x0040100401;
const L_WALL: u64 = 0x004010040100401;
const L_WALL2: u64 = L_WALL | L_WALL << 1;
const R_WALL: u64 = 0x802008020080200;
const R_WALL2: u64 = R_WALL | R_WALL >> 1;
const FLOOR: u64 = 0x3ff;
const FLOOR2: u64 = FLOOR | FLOOR << 10;

/// Board is represented using a bitboard
/// Based on wirelyre's idea
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

// TODO Bitboard struct
/// Read a bitboard from a string. Trailing and leading whitespace is ignored. Bitboards are read
/// as rows of '#'s and '.'s. Each row has exactly 10 of these characters, and each board has at
/// most 6 rows. The output of a call to `print_bitboard` is a valid input to this function.
///
/// # Examples
///
/// ```
/// # use fast_pc_solver::four_line_board::bitboard_from_string;
/// let bb = bitboard_from_string("
/// ..........
/// ..........
/// ..........
/// ..........
/// ");
/// assert_eq!(bb, 0);
///
/// let bb = bitboard_from_string("
/// #.........
/// #.........
/// #.........
/// #.........
/// ");
/// assert_eq!(bb, 0x0040100401);
/// ```
///
pub fn bitboard_from_string(s: &str) -> u64 {
    let s = s.trim();
    let mut r = 0;
    assert!(
        s.lines().count() <= 6,
        "Bitboards can be at most 6 lines long."
    );
    for l in s.lines() {
        let l = l.trim();
        assert!(
            l.len() == 10,
            "Bitboard lines must be exactly 10 characters long"
        );
        for c in l.chars().rev() {
            r <<= 1;
            match c {
                '#' => r |= 1,
                '.' => r |= 0,
                _ => panic!(
                    "Character {c} is not a valid square in a bitboard (only # or . are valid)"
                ),
            }
        }
    }
    r
}

// TODO queue struct instead of this thing lol
/// Converts a vector of minos into a queue as represented in a `FourLineBoard`. Queues
/// are represented as a 32 bit integer. In this representation, the pieces are stored in groups of
/// 3 bits, where each 3 bits encodes a piece. Since at most 10 pieces can be stored, it is
/// required that the input vector contains at most 10 minos. Empty entries into the queue are
/// represented with all 1s.
///
/// # Examples
///
/// ```
/// # use fast_pc_solver::four_line_board::queue_from_vec;
/// # use fast_pc_solver::types::Mino::*;
/// let q = queue_from_vec(vec![]);
/// assert_eq!(q, u32::MAX);
///
/// let q = queue_from_vec(vec![I, T, S, Z]);
/// assert_eq!(q, u32::MAX << 12 | 6 << 9 | 5 << 6 | 1 << 3 | 0);
/// ```
pub fn queue_from_vec(v: Vec<Mino>) -> u32 {
    assert!(
        v.len() <= 10,
        "Queues in FourLineBoards can have at most 10 pieces."
    );

    let mut queue = u32::MAX;

    for piece in v.iter().rev() {
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

    queue
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

// TODO consider not using `FourLineMove`s and instead return new `FourLineBoard`s when generating
// moves.
/// A move that can be made on a `FourLineBoard`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FourLineMove {
    did_hold: bool,
    piece: Mino,
    rotation: Rotation,
    x: u8,
    y: u8,
}

impl FourLineMove {
    // TODO don't do this lol
    /// Returns a mask representation of the piece this move will place, at the left most position,
    /// then its minimum x coordinate, minimum y coordinate, and maximum x coordinate values.
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

    /// Determines whether the given move places a piece within the bounds of a four line board.
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

    /// Determine whether the given move will result in a collision with the board.
    fn collision(&self, board: &FourLineBoard) -> bool {
        let (mut mask, min_x, min_y, max_x) = self.piece_mask();

        if self.x > max_x || self.x < min_x || self.y < min_y || self.y > 10 {
            return true;
        }

        if self.y >= 6 {
            return false;
        }

        mask <<= (self.x - min_x + 10 * (self.y - min_y)) % 64;

        (mask & board.board) != 0
    }

    /// Attempt to apply the specified shift to this move to get a new move.
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

    /// Attempt to apply the specified rotation to this move to get a new move, applying SRS
    /// kick rules.
    fn rotate(&self, board: &FourLineBoard, rotation: Spin) -> Option<Self> {
        let rot = self.rotation.apply(&rotation);

        let srs_table = self.piece.srs_table(self.rotation, rotation);

        for srs_entry in srs_table {
            let mv = FourLineMove {
                rotation: rot,
                x: ((self.x as i8) + srs_entry.0) as u8,
                y: ((self.y as i8) + srs_entry.1) as u8,
                ..*self
            };
            if !mv.collision(board) {
                return Some(mv);
            }
        }

        None
    }

    /// Apply a single step of gravity to this move.
    fn drop(&self, board: &FourLineBoard) -> Option<Self> {
        if self.y == 0 {
            return None;
        }
        let mv = FourLineMove {
            y: self.y - 1,
            ..*self
        };
        (!mv.collision(board)).then_some(mv)
    }
}

impl FourLineBoard {
    // TODO Fix this lol
    // NOTE (18/04/2025):
    //      fix what??!?!
    /// Return the resulting piece after applying a hold, if there is anything left in the queue.
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

    /// Return the next piece in the queue
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

    /// Return the 2nd piece in the queue
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

    /// Return the y position of the given move after performing a hard drop.
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
    /// Apply the given move to get a new board.
    fn make_move(self, mv: FourLineMove) -> FourLineBoard {
        let (piece_mask, min_x, min_y, _) = mv.piece_mask();

        debug_assert!(mv.x >= min_x, "mv: {:?}\nmin_x: {}", mv, min_x);
        debug_assert!(mv.y >= min_y, "mv: {:?}\nmin_y: {}", mv, min_y);
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

    /// Determines whether a perfect clear has been found
    fn solved(&self) -> bool {
        debug_assert!(self.cleared <= 4);

        self.cleared == 4
    }
}

/// An implementation of a move generation algorithm. Implementors should be able to iterate over
/// all moves that can be made on its input board.
pub trait FourLineMoveGenerator: Iterator<Item = FourLineMove> {
    /// Create a generator which will generate moves for the given board.
    fn new(board: FourLineBoard) -> Self;
}

/// A move generator that performs a DFS incrementally until all moves are found.
pub struct SearchFourLineMoveGenerator {
    board: FourLineBoard,
    stack: Vec<FourLineMove>,
    table: FxHashSet<FourLineMove>,
    hd_table: FxHashSet<u64>,
}

impl FourLineMoveGenerator for SearchFourLineMoveGenerator {
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
                y: 5,
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
                y: 5,
            };
            stack.push(mv);
            table.insert(mv);
        }

        SearchFourLineMoveGenerator {
            board,
            stack,
            table,
            hd_table,
        }
    }
}

impl Iterator for SearchFourLineMoveGenerator {
    type Item = FourLineMove;

    fn next(&mut self) -> Option<Self::Item> {
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
            for spin in [Spin::Clockwise, Spin::AntiClockwise /*, Spin::Half*/] {
                if let Some(mv) = mv.rotate(&self.board, spin) {
                    if !self.table.contains(&mv) {
                        self.stack.push(mv);
                        self.table.insert(mv);
                    }
                }
            }
            if let Some(mv) = mv.drop(&self.board) {
                if !self.table.contains(&mv) {
                    self.stack.push(mv);
                    self.table.insert(mv);
                }
            }

            let drop = FourLineMove {
                y: self.board.drop_y(mv),
                ..mv
            };

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

use std::simd::prelude::*;

// TODO explain this properly
/// A move generator that uses epic bit manipulation tricks and stuff and simd and is really cool
/// and super fast
pub struct BitwiseMoveGenerator {
    cur_moves: [u64; 4],
    hold_moves: [u64; 4],
    hold: bool,
    idx: usize,
    piece: Option<Mino>,
    hold_piece: Option<Mino>,
}

/// Computes srs kick data from srs offsets
const fn srs_data(
    offsets: [[(i8, i8); 4]; 5],
    rotation: usize,
    step: usize,
    dir: usize,
) -> (i8, i8) {
    let from = (dir + 3 + 2 * rotation) % 4;
    let (ax, ay) = offsets[step][from];
    let (bx, by) = offsets[step][dir];
    (ax - bx, ay - by)
}

/// Computes an srs kick data table given an srs offset table
const fn table_result(
    offsets: [[(i8, i8); 4]; 5],
) -> ([[[u8; 4]; 5]; 2], [[[u8; 4]; 5]; 2], [[[u64; 4]; 5]; 2]) {
    // TODO this is still quite ugly make this look better please thank you
    (
        from_fn!(|rotation| {
            from_fn!(|step| {
                from_fn!(|dir| {
                    let (dx, dy) = srs_data(offsets, rotation, step, dir);
                    let shift = dx + 10 * dy;
                    if shift >= 0 {
                        shift as u8
                    } else {
                        0
                    }
                })
            })
        }),
        from_fn!(|rotation| {
            from_fn!(|step| {
                from_fn!(|dir| {
                    let (dx, dy) = srs_data(offsets, rotation, step, dir);
                    let shift = dx + 10 * dy;
                    if shift <= 0 {
                        (-shift) as u8
                    } else {
                        0
                    }
                })
            })
        }),
        from_fn!(|rotation| {
            from_fn!(|step| {
                from_fn!(|dir| {
                    let (dx, _) = srs_data(offsets, rotation, step, dir);
                    match dx {
                        -2 => L_WALL2,
                        -1 => L_WALL,
                        0 => 0,
                        1 => R_WALL,
                        2 => R_WALL2,
                        _ => unreachable!(),
                    }
                })
            })
        }),
    )
}

/// Gets the srs table of the given piece. TODO can I be bothered explaining the type signature?!
const fn srs_table(piece: Mino) -> ([[[u8; 4]; 5]; 2], [[[u8; 4]; 5]; 2], [[[u64; 4]; 5]; 2]) {
    const I_RESULT: ([[[u8; 4]; 5]; 2], [[[u8; 4]; 5]; 2], [[[u64; 4]; 5]; 2]) =
        table_result(I_SRS_OFFSETS);
    const JLSTZ_RESULT: ([[[u8; 4]; 5]; 2], [[[u8; 4]; 5]; 2], [[[u64; 4]; 5]; 2]) =
        table_result(JLSTZ_SRS_OFFSETS);

    match piece {
        Mino::O => ([[[0; 4]; 5]; 2], [[[0; 4]; 5]; 2], [[[0; 4]; 5]; 2]),
        Mino::I => I_RESULT,
        _ => JLSTZ_RESULT,
    }
}

/// Compute the relevant occupancy marks for valid piece center squares.
fn gen_occs(board: u64, piece: Mino) -> u64x4 {
    let board_l2 = board & !L_WALL2;
    let board_l = board & !L_WALL;
    let board_r2 = board & !R_WALL2;
    let board_r = board & !R_WALL;
    // I WISH I GENERATED THIS OH MY GOD
    match piece {
        Mino::O => u64x4::from_array([
            board | board_l >> 1 | board >> 10 | board_l >> 11 | R_WALL,
            0xffffffffff,
            0xffffffffff,
            0xffffffffff,
        ]),
        Mino::I => u64x4::from_array([
            board_l2 >> 2 | board_l >> 1 | board | board_r << 1 | L_WALL | R_WALL2,
            board >> 10 | board | board << 10 | board << 20 | FLOOR2,
            board_l >> 1 | board | board_r << 1 | board_r2 << 2 | L_WALL2 | R_WALL,
            board >> 20 | board >> 10 | board | board << 10 | FLOOR,
        ]),
        Mino::T => u64x4::from_array([
            board >> 10 | board_l >> 1 | board | board_r << 1 | L_WALL | R_WALL,
            board >> 10 | board | board_l >> 1 | board << 10 | FLOOR | R_WALL,
            board_l >> 1 | board | board_r << 1 | board << 10 | FLOOR | L_WALL | R_WALL,
            board >> 10 | board_r << 1 | board | board << 10 | FLOOR | L_WALL,
        ]),
        Mino::J => u64x4::from_array([
            board_r >> 9 | board_l >> 1 | board | board_r << 1 | L_WALL | R_WALL,
            board >> 10 | board_l >> 11 | board | board << 10 | FLOOR | R_WALL,
            board_l >> 1 | board | board_r << 1 | board_l << 9 | FLOOR | L_WALL | R_WALL,
            board >> 10 | board | board_r << 11 | board << 10 | FLOOR | L_WALL,
        ]),
        Mino::L => u64x4::from_array([
            board_l >> 11 | board_l >> 1 | board | board_r << 1 | L_WALL | R_WALL,
            board >> 10 | board | board << 10 | board_l << 9 | FLOOR | R_WALL,
            board_l >> 1 | board | board_r << 1 | board_r << 11 | FLOOR | L_WALL | R_WALL,
            board_r >> 9 | board >> 10 | board | board << 10 | FLOOR | L_WALL,
        ]),
        Mino::S => u64x4::from_array([
            board >> 10 | board_l >> 11 | board_r << 1 | board | L_WALL | R_WALL,
            board >> 10 | board | board_l >> 1 | board_l << 9 | FLOOR | R_WALL,
            board << 10 | board_r << 11 | board_l >> 1 | board | FLOOR | L_WALL | R_WALL,
            board << 10 | board | board_r << 1 | board_r >> 9 | FLOOR | L_WALL,
        ]),
        Mino::Z => u64x4::from_array([
            board_r >> 9 | board >> 10 | board | board_l >> 1 | L_WALL | R_WALL,
            board_l >> 11 | board | board_l >> 1 | board << 10 | FLOOR | R_WALL,
            board_l << 9 | board << 10 | board | board_r << 1 | FLOOR | L_WALL | R_WALL,
            board_r << 11 | board | board_r << 1 | board >> 10 | FLOOR | L_WALL,
        ]),
    }
}

/// Generate masks of valid piece placements in a four line board accounting for the maximum
/// height of `four - cleared`.
fn gen_heights(cleared: u8, piece: Mino) -> u64x4 {
    let board = 0xffffffffff >> (10 * cleared);
    match piece {
        Mino::O => u64x4::splat(board >> 10),
        Mino::I => u64x4::from_array([board, board >> 10, board, board >> 20]),
        Mino::T => u64x4::from_array([board >> 10, board >> 10, board, board >> 10]),
        Mino::J => u64x4::from_array([board >> 10, board >> 10, board, board >> 10]),
        Mino::L => u64x4::from_array([board >> 10, board >> 10, board, board >> 10]),
        Mino::S => u64x4::from_array([board >> 10, board >> 10, board, board >> 10]),
        Mino::Z => u64x4::from_array([board >> 10, board >> 10, board, board >> 10]),
    }
}

/// Print a bitboard as a grid of '#'s and '.'s.
#[allow(unused)]
fn print_bitboard(bb: u64) {
    for y in (0..6).rev() {
        for x in 0..10 {
            if bb & (1 << (y * 10 + x)) != 0 {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}

/// Generate a vector of boards representing valid piece placements.
fn bitwise_gen(board: u64, cleared: u8, piece: Option<Mino>) -> [u64; 4] {
    let Some(piece) = piece else {
        return [0; 4];
    };

    let (left_srs, right_srs, srs_masks) = srs_table(piece);

    let occs = gen_occs(board, piece);
    let heights = gen_heights(cleared, piece);
    // Start with the pieces above the board. We work with a simd vector of 4 boards, representing
    // each rotation of the piece.
    let mut moves = !occs & u64x4::splat(0x3FF << 40);
    let mut last = u64x4::splat(0);

    let left_wall = u64x4::splat(0x004010040100401);
    let right_wall = u64x4::splat(0x802008020080200);

    // We will repeatedly try and find new moves until no new moves are found.
    while moves != last {
        last = moves;

        // The first step is to find all shifts and soft drops. This is done with a bitwise flood
        // fill!
        let mut last_fill = u64x4::splat(0);
        while last_fill != moves {
            last_fill = moves;
            moves |= (moves >> 10) & !occs;
            moves |= ((moves & !left_wall) >> 1) & !occs;
            moves |= ((moves & !right_wall) << 1) & !occs;
        }

        // Next we do rotations, applying srs kick rules.
        // please unroll this loop compiler thanks
        for dir in 0..2 {
            // We will clear out the bits in rotating that pass an srs test.
            let mut rotating = if dir == 0 {
                simd_swizzle!(moves, [3, 0, 1, 2])
            } else {
                simd_swizzle!(moves, [1, 2, 3, 0])
            };
            let mut new_moves = u64x4::splat(0);
            for test in 0..5 {
                let ls = u8x4::from_array(left_srs[dir][test]).cast();
                let rs = u8x4::from_array(right_srs[dir][test]).cast();
                let mask = u64x4::from_array(srs_masks[dir][test]);

                let moved = ((rotating & !mask) << ls >> rs) & !occs;
                rotating &= !(moved >> ls << rs);
                new_moves |= moved;
            }
            moves |= new_moves;
        }
    }

    // We allow hard drops only (no floating placements). To do this we remove squares which have
    // placements below them.
    moves &= !(moves << 10);

    // Remove moves which place pieces the same way
    match piece {
        Mino::I => {
            moves[0] |= moves[2] >> 1;
            moves[1] |= moves[3] << 10;
            moves[2] = 0;
            moves[3] = 0;
        }
        Mino::S | Mino::Z => {
            moves[0] |= moves[2] >> 10;
            moves[1] |= moves[3] >> 1;
            moves[2] = 0;
            moves[3] = 0;
        }
        _ => {}
    }

    // Remove pieces that do not fit in the board.
    moves &= heights;

    moves.to_array()
}

/*
#[test]
fn test_bitwise() {
    // Queue OTJJOL
    use super::{
        interface_board::Board,
        types::{CellColour::*, Mino::*},
    };

    let mut board = Board {
        grid: [[CellColour::EMPTY; 10]; 40],
        hold: None,
        piece: O,
        queue: vec![J, T, S, Z, I],
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

    let four_line = board.to_four_line().unwrap();

    BitwiseMoveGenerator::new(four_line);
}
*/

impl FourLineMoveGenerator for BitwiseMoveGenerator {
    fn new(board: FourLineBoard) -> Self {
        let cur_moves = bitwise_gen(board.board, board.cleared, board.piece);
        let hold_moves = if board.piece != board.hold() {
            bitwise_gen(board.board, board.cleared, board.hold())
        } else {
            [0; 4]
        };

        BitwiseMoveGenerator {
            cur_moves,
            hold_moves,
            hold: false,
            idx: 0,
            piece: board.piece,
            hold_piece: board.hold(),
        }
    }
}

impl Iterator for BitwiseMoveGenerator {
    type Item = FourLineMove;

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < 4 {
            let did_hold = self.hold;
            let Some(piece) = (if !self.hold {
                self.piece
            } else {
                self.hold_piece
            }) else {
                if !self.hold {
                    self.hold = true;
                    continue;
                } else {
                    break;
                }
            };
            let rotation = match self.idx {
                0 => Rotation::North,
                1 => Rotation::East,
                2 => Rotation::South,
                3 => Rotation::West,
                _ => return None,
            };

            let cur = if !self.hold {
                &mut self.cur_moves[self.idx]
            } else {
                &mut self.hold_moves[self.idx]
            };

            if *cur == 0 {
                self.idx += 1;
                if !self.hold && self.idx >= 4 {
                    self.hold = true;
                    self.idx = 0;
                }
                continue;
            }

            // pop the lsb as our target square
            let square = cur.trailing_zeros();
            *cur = *cur & (cur.wrapping_sub(1));

            let x = (square % 10) as u8;
            let y = (square / 10) as u8;

            return Some(FourLineMove {
                did_hold,
                piece,
                rotation,
                x,
                y,
            });
        }
        None
    }
}

/// A move generator which runs two move generators, and compares their results. The purpose of
/// this is to test that two generators have equivalent behaviour.
struct MoveGenTester<A: FourLineMoveGenerator, B: FourLineMoveGenerator> {
    moves: Vec<FourLineMove>,
    _a: PhantomData<A>,
    _b: PhantomData<B>,
}

impl<A: FourLineMoveGenerator, B: FourLineMoveGenerator> Iterator for MoveGenTester<A, B> {
    type Item = FourLineMove;

    fn next(&mut self) -> Option<Self::Item> {
        self.moves.pop()
    }
}

impl<A: FourLineMoveGenerator, B: FourLineMoveGenerator> FourLineMoveGenerator
    for MoveGenTester<A, B>
{
    fn new(board: FourLineBoard) -> Self {
        use std::collections::{BTreeMap, BTreeSet, HashSet};
        let a_moves = A::new(board)
            .map(|m| (board.make_move(m), m))
            .collect::<BTreeMap<_, _>>();
        let b_moves = B::new(board)
            .map(|m| (board.make_move(m), m))
            .collect::<BTreeMap<_, _>>();

        if a_moves.keys().map(|b| b.board).collect::<HashSet<_>>()
            != b_moves.keys().map(|b| b.board).collect::<HashSet<_>>()
        {
            print_bitboard(board.board);

            println!("{:?} {:?} {:?}", board.piece, board.hold, board.queue);

            println!("first: {:?}", a_moves.values().collect::<BTreeSet<_>>());
            println!("second: {:?}", b_moves.values().collect::<BTreeSet<_>>());

            println!(
                "f - s: {:?}",
                //a_moves.difference(&b_moves).collect::<Vec<_>>()
                a_moves
                    .iter()
                    .filter_map(|(b, m)| { (!b_moves.contains_key(&b)).then_some(m) })
                    .collect::<Vec<_>>()
            );
            println!(
                "s - f: {:?}",
                b_moves
                    .iter()
                    .filter_map(|(b, m)| { (!a_moves.contains_key(&b)).then_some(m) })
                    .collect::<Vec<_>>()
            );

            panic!("Not equal!");
        }

        let moves = a_moves.into_iter().map(|(_, m)| m).collect();
        Self {
            moves,
            _a: PhantomData,
            _b: PhantomData,
        }
    }
}

impl FourLineBoard {
    /// Find for a perfect clear with a brute force search. Optionally allow pruning for a speedup
    /// (noting that it currently has not been rigorously tested!)
    pub fn solution<M: FourLineMoveGenerator, const PRUNE: bool>(
        &mut self,
    ) -> Option<Vec<FourLineMove>> {
        if self.solved() {
            return Some(vec![]);
        }

        if PRUNE && self.cannot_pc() {
            return None;
        }

        let mut move_generator = M::new(*self);

        while let Some(mv) = move_generator.next() {
            let mut new_board = self.make_move(mv);
            if let Some(mut sol) = new_board.solution::<M, PRUNE>() {
                let mut vec = vec![mv];

                vec.append(&mut sol);

                return Some(vec);
            }
        }
        None
    }

    /// Heuristics that determine whether a board cannot get a perfect clear. If this method
    /// returns true, the board cannot have a pc, but if the method does not return true no
    /// information can be known (i.e. it could be possible, or could be impossible to pc).
    pub fn cannot_pc(&self) -> bool {
        if self.board.count_ones() <= 28 {
            // Connected regions
            let separators = self.region_separators();

            let col = COLUMN >> (10 * self.cleared);

            let mut sqs = 0;
            for i in 0..10 {
                let w = col << i;
                sqs += (self.board & w).count_zeros() - w.count_zeros();
                if separators & w == w || i == 9 {
                    if sqs % 4 != 0 {
                        //println!("{i} {sqs}");
                        return true;
                    }
                    sqs = 0;
                }
            }
        }

        false
    }

    /// Computes a mask of separators of regions. A column being filled means that column onwards
    /// contains a new region.
    pub fn region_separators(&self) -> u64 {
        let cols = self.board | (self.board & !L_WALL) >> 1;
        let col = COLUMN >> (10 * self.cleared);

        let mut separators = 0;

        for i in 0..10 {
            let w = col << i;
            if cols & w == w {
                separators |= w;
            }
        }

        //print_bitboard(cols);
        //print_bitboard(separators);

        separators
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

        let solution = four_line.solution::<SearchFourLineMoveGenerator, false>();

        println!("Moves are: {:?}", solution);

        assert!(solution.is_some());
    }

    #[test]
    fn srs() {
        let mut board = Board {
            grid: [[CellColour::EMPTY; 10]; 40],
            hold: None,
            piece: Z,
            queue: vec![],
        };
        board.grid[3] = [
            CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY, EMPTY, EMPTY, EMPTY,
        ];
        board.grid[2] = [CYAN, CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY, CYAN, CYAN, CYAN];
        board.grid[1] = [CYAN, CYAN, CYAN, CYAN, CYAN, EMPTY, EMPTY, CYAN, CYAN, CYAN];
        board.grid[0] = [
            CYAN, CYAN, CYAN, CYAN, EMPTY, EMPTY, EMPTY, CYAN, CYAN, CYAN,
        ];

        let four_line = board.to_four_line().unwrap();

        print_bitboard(four_line.board);

        let mv = FourLineMove {
            did_hold: false,
            piece: Z,
            rotation: Rotation::North,
            x: 6,
            y: 3,
        };
        assert_eq!(
            mv.rotate(&four_line, Spin::AntiClockwise),
            Some(FourLineMove {
                did_hold: false,
                piece: Z,
                rotation: Rotation::West,
                x: 7,
                y: 3
            })
        );

        let moves = SearchFourLineMoveGenerator::new(four_line).collect::<Vec<_>>();

        println!("Moves are: {:?}", moves);

        assert!(moves.is_empty());
    }

    fn test_board(board: u64, queues: Vec<(Option<Mino>, Option<Mino>, Vec<Mino>)>) {
        for (piece, hold, queue) in queues {
            let mut board = FourLineBoard {
                board,
                hold,
                piece,
                queue: queue_from_vec(queue),
                cleared: 0,
            };

            let _ = board
                .solution::<MoveGenTester<SearchFourLineMoveGenerator, BitwiseMoveGenerator>, false>();

            let no_prune = board
                .solution::<BitwiseMoveGenerator, false>();
            let prune = board
                .solution::<BitwiseMoveGenerator, true>();
            assert_eq!(no_prune, prune);
        }
    }

    #[test]
    fn boards() {
        let board = bitboard_from_string(
            "
            ####......
            ###.......
            #####.....
            ####......
            ",
        );

        // Cases commented out for performance reasons lol this test is slow (TODO make the choice
        // to run the slow tests a user option)
        let queues = vec![
            (Some(O), None, vec![J, T, I, Z, L]),
            (Some(S), None, vec![J, T, I, I, I]),
            (Some(Z), None, vec![T, L, O, O, S]),
            (Some(O), None, vec![Z, T, L, I, O]),
        ];

        test_board(board, queues);

        let board = bitboard_from_string(
            "
            ###.......
            ###.......
            ####......
            ######....
            ",
        );

        let queues = vec![
            (Some(S), None, vec![T, J, I, O, L]),
            (Some(O), None, vec![L, O, I, T, J]),
        ];

        test_board(board, queues);

        let board = bitboard_from_string(
            "
            #.......##
            #........#
            ###.....##
            ###....###
            ",
        );

        let queues = vec![
            (Some(Z), None, vec![J, S, I, O, L]),
            (Some(J), None, vec![S, T, Z, L, I]),
        ];

        test_board(board, queues);
    }

    #[test]
    fn separators() {
        let board = bitboard_from_string(
            "
            ####.##..#
            ###..###.#
            ####.###.#
            ",
        );
        let board = FourLineBoard {
            board,
            hold: None,
            piece: None,
            queue: 0,
            cleared: 1,
        };
        // TODO the wrong thing is being tested here, but I was too lazy to do it properly
        // This board in fact cannot pc, but what I should be checking for is that it passes the
        // separator test, since this board does satisfy the requirement of having regions having
        // congruent to 0 mod 4 empty squares.
        assert!(!board.cannot_pc());
    }
}
