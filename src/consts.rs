// Source:
// https://web.archive.org/web/20250105151246/https://harddrop.com/wiki/SRS#How_Guideline_SRS_Really_Works
// 4 element arrays are indexed north, east, south then west.
//pub const O_SRS_OFFSETS: [(i32, i32); 4] = [(0, 0), (0, -1), (-1, -1), (-1, 0)];
pub const I_SRS_OFFSETS: [[(i32, i32); 4]; 5] = [
    [(0, 0), (-1, 0), (-1, 1), (0, 1)],
    [(-1, 0), (0, 0), (1, 1), (0, 1)],
    [(2, 0), (0, 0), (-2, 1), (0, 1)],
    [(-1, 0), (0, 1), (1, 0), (0, -1)],
    [(2, 0), (0, -2), (-2, 0), (0, 2)],
];
pub const JLSTZ_SRS_OFFSETS: [[(i32, i32); 4]; 5] = [
    [(0, 0), (0, 0), (0, 0), (0, 0)],
    [(0, 0), (1, 0), (0, 0), (-1, 0)],
    [(0, 0), (1, -1), (0, 0), (-1, -1)],
    [(0, 0), (0, 2), (0, 0), (0, 2)],
    [(0, 0), (1, 2), (0, 0), (-1, 2)],
];
