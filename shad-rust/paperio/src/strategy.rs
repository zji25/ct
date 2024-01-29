use crate::data::{Cell, Direction, Player, World};
use num_traits::abs;
use std::cmp::{max, min};

////////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
pub struct Rectangle {
    ax: i32,
    ay: i32,
    bx: i32,
    by: i32,
}
pub struct Strategy {
    best_rectangle: Rectangle,
    previous_direction: Direction,
}

impl Rectangle {
    pub fn new(cell1: &Cell, cell2: &Cell) -> Self {
        Self {
            ax: min(cell1.0, cell2.0),
            ay: min(cell1.1, cell2.1),
            bx: max(cell1.0, cell2.0),
            by: max(cell1.1, cell2.1),
        }
    }
}
impl Default for Strategy {
    fn default() -> Self {
        Self::new()
    }
}

impl Strategy {
    pub fn new() -> Self {
        Self {
            best_rectangle: Rectangle::new(&Cell(0, 0), &Cell(0, 0)),
            previous_direction: Direction::Down,
        }
    }

    fn get_distance(cell: &Cell, rec: &Rectangle) -> i32 {
        if rec.ax <= cell.0 && cell.0 <= rec.bx && rec.ay <= cell.1 && cell.1 <= rec.by {
            return 0;
        }
        if rec.ax <= cell.0 && cell.0 <= rec.bx {
            return min(abs(rec.ay - cell.1), abs(rec.by - cell.1));
        }
        if rec.ay <= cell.1 && cell.1 <= rec.by {
            return min(abs(rec.ax - cell.0), abs(rec.bx - cell.0));
        }
        [
            Cell(rec.ax, rec.ay),
            Cell(rec.bx, rec.by),
            Cell(rec.ax, rec.by),
            Cell(rec.bx, rec.ay),
        ]
        .iter()
        .map(|corner| cell.distance_to(*corner))
        .min()
        .unwrap()
    }

    fn get_score(current_cell: &Cell, cell: &Cell, world: &World) -> i32 {
        if current_cell.0 == cell.0 || current_cell.1 == cell.1 {
            return 0;
        }
        let mut enemy_cells = 0;
        let mut empty_cells = 0;
        world
            .iter_cells()
            .filter(|c| {
                min(current_cell.0, cell.0) <= c.0
                    && c.0 <= max(current_cell.0, cell.0)
                    && min(current_cell.1, cell.1) <= c.1
                    && c.1 <= max(current_cell.1, cell.1)
            })
            .for_each(|rc| {
                if world
                    .iter_enemies()
                    .any(|enemy| enemy.1.territory.iter().any(|ec| ec.to_cell() == rc))
                {
                    enemy_cells += 1;
                } else if !world.me().territory.iter().any(|mc| mc.to_cell() == rc) {
                    empty_cells += 1;
                }
            });
        let cell_score = enemy_cells * 5 + empty_cells;
        let min_enemy_distance = world
            .iter_enemies()
            .map(|enemy| {
                Self::get_distance(
                    &enemy.1.position.to_cell(),
                    &Rectangle::new(current_cell, cell),
                )
            })
            .min()
            .unwrap();

        let danger =
            (abs(current_cell.0 - cell.0) + abs(current_cell.1 - cell.1)) * 2 - min_enemy_distance;
        cell_score * 4 - danger * danger
    }

    fn is_on_the_perimeter(&self, cell: Cell) -> bool {
        (self.best_rectangle.ax == cell.0 || self.best_rectangle.bx == cell.0)
            && self.best_rectangle.ay <= cell.1
            && cell.1 <= self.best_rectangle.by
            || (self.best_rectangle.ay == cell.1 || self.best_rectangle.by == cell.1)
                && self.best_rectangle.ax <= cell.0
                && cell.0 <= self.best_rectangle.bx
    }

    pub fn on_tick(&mut self, world: World) -> Direction {
        let me: &Player = world.me();
        let my_cell = me.position.to_cell();
        if me.territory.iter().any(|point| point == &me.position) {
            let best_cell = world
                .iter_cells()
                .map(|cell| (cell, Self::get_score(&my_cell, &cell, &world)))
                .fold((my_cell, i32::MIN), |mx, x| {
                    if mx.1 > x.1 {
                        return mx;
                    }
                    x
                });
            self.best_rectangle = Rectangle::new(&my_cell, &best_cell.0);
            let dx = best_cell.0 .0 - my_cell.0;
            let dy = best_cell.0 .1 - my_cell.1;
            if dx > 0 && self.previous_direction != Direction::Left {
                self.previous_direction = Direction::Right;
                return Direction::Right;
            }
            if dx < 0 && self.previous_direction != Direction::Right {
                self.previous_direction = Direction::Left;
                return Direction::Left;
            }
            if dy > 0 && self.previous_direction != Direction::Down {
                self.previous_direction = Direction::Up;
                return Direction::Up;
            }
            if dy < 0 && self.previous_direction != Direction::Up {
                self.previous_direction = Direction::Down;
                return Direction::Down;
            }
        }
        let adj_pd = my_cell.adjacent(self.previous_direction);
        if let Some(x) = adj_pd {
            if self.is_on_the_perimeter(x) {
                return self.previous_direction;
            }
        }
        let new_direction = self.previous_direction.next(true);
        let adj_pd = my_cell.adjacent(new_direction);
        if let Some(x) = adj_pd {
            if self.is_on_the_perimeter(x) {
                self.previous_direction = new_direction;
                return self.previous_direction;
            }
        }
        self.previous_direction = new_direction.opposite();
        self.previous_direction
    }
}
