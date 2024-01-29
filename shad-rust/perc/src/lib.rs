#![forbid(unsafe_code)]

use rand::prelude::*;

pub struct BoolGrid {
    width: usize,
    height: usize,
    data: Vec<Vec<bool>>,
}

impl BoolGrid {
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            data: vec![vec![false; width]; height],
        }
    }
    pub fn random(width: usize, height: usize, vacancy: f64) -> Self {
        let mut grid = BoolGrid::new(width, height);
        let mut rng = rand::thread_rng();
        for i in 0..height {
            for j in 0..width {
                let x: f64 = rng.gen();
                if x > vacancy {
                    grid.set(j, i, true);
                }
            }
        }
        grid
    }
    pub fn width(&self) -> usize {
        self.width
    }
    pub fn height(&self) -> usize {
        self.height
    }
    pub fn data(&self) -> Vec<Vec<bool>> {
        self.data.clone()
    }
    pub fn get(&self, x: usize, y: usize) -> bool {
        self.data[y][x]
    }
    pub fn set(&mut self, x: usize, y: usize, value: bool) {
        self.data[y][x] = value;
    }
}

pub fn dfs(i: usize, j: usize, grid: &BoolGrid, visited: &mut Vec<Vec<bool>>) -> bool {
    if grid.get(j, i) {
        return false;
    }
    if i == grid.height() - 1 {
        return true;
    }
    visited[i][j] = true;

    i > 0 && !visited[i - 1][j] && dfs(i - 1, j, grid, visited)
        || i < grid.height() - 1 && !visited[i + 1][j] && dfs(i + 1, j, grid, visited)
        || j > 0 && !visited[i][j - 1] && dfs(i, j - 1, grid, visited)
        || j < grid.width() - 1 && !visited[i][j + 1] && dfs(i, j + 1, grid, visited)
}
pub fn percolates(grid: &BoolGrid) -> bool {
    if grid.width() == 0 || grid.height() == 0 {
        return true;
    }
    let mut visited: Vec<Vec<bool>>;
    for j in 0..grid.width() {
        visited = vec![vec![false; grid.width()]; grid.height()];
        if dfs(0, j, grid, &mut visited) {
            return true;
        }
    }
    return false;
}

const N_TRIALS: u64 = 10000;

pub fn evaluate_probability(width: usize, height: usize, vacancy: f64) -> f64 {
    let mut perc_count = 0;
    for _ in 0..N_TRIALS {
        let grid = BoolGrid::random(width, height, vacancy);
        if percolates(&grid) {
            perc_count += 1;
        }
    }
    return perc_count as f64 / N_TRIALS as f64;
}
