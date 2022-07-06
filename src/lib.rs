use crate::four_line_board::*;
use crate::solver::*;
mod four_line_board;
mod solver;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
