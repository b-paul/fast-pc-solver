use criterion::{criterion_group, criterion_main, Criterion};
use fast_pc_solver::{four_line_board::*, interface_board::*, types::CellColour::*, types::Mino::*, types::*};

pub fn criterion_benchmark(c: &mut Criterion) {
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

    c.bench_function("random jaws", |b| {
        b.iter(|| {
            let _ = four_line.solution::<SearchFourLineMoveGenerator>();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
