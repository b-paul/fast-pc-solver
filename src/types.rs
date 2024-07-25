#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CellColour {
    EMPTY,
    CYAN,
    PURPLE,
    YELLOW,
    BLUE,
    ORANGE,
    GREEN,
    RED,
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Mino {
    I,
    T,
    O,
    J,
    L,
    S,
    Z,
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Rotation {
    North,
    East,
    South,
    West,
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum ClearType {
    None,
    Mini,
    Full,
}
#[derive(Debug, Clone, Copy)]
pub enum Shift {
    TapLeft,
    TapRight,
}
#[derive(Debug, Clone, Copy)]
pub enum Spin {
    Clockwise,
    AntiClockwise,
    Half,
}
impl Mino {
    pub const fn srs_table(&self, rotation: Rotation, spin: Spin) -> [(i8, i8); 5] {
        // https://gist.github.com/torchlight/1832786cf053daa51bf188110b764090#file-minorotationimpl-java-L140=
        // Optimization: dont check duplicate srs multiple times
        match self {
            Mino::O => match rotation {
                Rotation::North => match spin {
                    Spin::Clockwise => [(0, 1); 5],
                    Spin::AntiClockwise => [(1, 0); 5],
                    Spin::Half => [(1, 1); 5],
                },
                Rotation::East => match spin {
                    Spin::Clockwise => [(1, 0); 5],
                    Spin::AntiClockwise => [(0, -1); 5],
                    Spin::Half => [(1, -1); 5],
                },
                Rotation::South => match spin {
                    Spin::Clockwise => [(0, -1); 5],
                    Spin::AntiClockwise => [(-1, 0); 5],
                    Spin::Half => [(-1, -1); 5],
                },
                Rotation::West => match spin {
                    Spin::Clockwise => [(-1, 0); 5],
                    Spin::AntiClockwise => [(0, 1); 5],
                    Spin::Half => [(-1, 1); 5],
                },
            },
            Mino::I => match rotation {
                Rotation::North => match spin {
                    Spin::Clockwise => [(1, 0), (-1, 0), (2, 0), (-1, -1), (2, 2)],
                    Spin::AntiClockwise => [(0, -1), (-1, -1), (2, -1), (-1, 1), (2, -2)],
                    Spin::Half => [(0, 0); 5],
                },
                Rotation::East => match spin {
                    Spin::Clockwise => [(0, -1), (-1, -1), (2, -1), (-1, 1), (2, -2)],
                    Spin::AntiClockwise => [(-1, 0), (1, 0), (-2, 0), (1, 1), (-2, -2)],
                    Spin::Half => [(0, 0); 5],
                },
                Rotation::South => match spin {
                    Spin::Clockwise => [(-1, 0), (1, 0), (-2, 0), (1, 1), (-2, -2)],
                    Spin::AntiClockwise => [(0, 1), (1, 1), (-2, 1), (1, -1), (-2, 2)],
                    Spin::Half => [(0, 0); 5],
                },
                Rotation::West => match spin {
                    Spin::Clockwise => [(0, 1), (1, 1), (-2, 1), (1, -1), (-2, 2)],
                    Spin::AntiClockwise => [(1, 0), (-1, 0), (2, 0), (-1, -1), (2, 2)],
                    Spin::Half => [(0, 0); 5],
                },
            },
            // TODO tetrio kicks
            // 180 might also be wrong
            _ => match rotation {
                Rotation::North => match spin {
                    Spin::Clockwise => [(0, 0), (-1, 0), (-1, 1), (0, -2), (-1, -2)],
                    Spin::AntiClockwise => [(0, 0), (1, 0), (1, 1), (0, -2), (1, -2)],
                    Spin::Half => [(0, 0), (0, 1), (0, 1), (0, 1), (0, 1)],
                },
                Rotation::East => match spin {
                    Spin::Clockwise => [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)],
                    Spin::AntiClockwise => [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)],
                    Spin::Half => [(0, 0), (1, 0), (1, 0), (1, 0), (1, 0)],
                },
                Rotation::South => match spin {
                    Spin::Clockwise => [(0, 0), (1, 0), (1, 1), (0, -2), (1, -2)],
                    Spin::AntiClockwise => [(0, 0), (-1, 0), (-1, 1), (0, -2), (-1, -2)],
                    Spin::Half => [(0, 0), (0, -1), (0, -1), (0, -1), (0, -1)],
                },
                Rotation::West => match spin {
                    Spin::Clockwise => [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)],
                    Spin::AntiClockwise => [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)],
                    Spin::Half => [(0, 0), (-1, 0), (-1, 0), (-1, 0), (-1, 0)],
                },
            },
        }
    }
}

impl Rotation {
    pub fn apply(&self, rotation: &Spin) -> Rotation {
        match *self {
            Rotation::North => match rotation {
                Spin::Clockwise => Rotation::East,
                Spin::AntiClockwise => Rotation::West,
                Spin::Half => Rotation::South,
            },
            Rotation::East => match rotation {
                Spin::Clockwise => Rotation::South,
                Spin::AntiClockwise => Rotation::North,
                Spin::Half => Rotation::West,
            },
            Rotation::South => match rotation {
                Spin::Clockwise => Rotation::West,
                Spin::AntiClockwise => Rotation::East,
                Spin::Half => Rotation::North,
            },
            Rotation::West => match rotation {
                Spin::Clockwise => Rotation::North,
                Spin::AntiClockwise => Rotation::South,
                Spin::Half => Rotation::East,
            },
        }
    }
}
