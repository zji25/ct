#![forbid(unsafe_code)]

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoundOutcome {
    BothCooperated,
    LeftCheated,
    RightCheated,
    BothCheated,
}

pub struct Game {
    left: Box<dyn Agent>,
    right: Box<dyn Agent>,
}

impl Game {
    pub fn new(left: Box<dyn Agent>, right: Box<dyn Agent>) -> Self {
        Self { left, right }
    }
    pub fn left_score(&self) -> i32 {
        self.left.score()
    }
    pub fn right_score(&self) -> i32 {
        self.right.score()
    }
    pub fn play_round(&mut self) -> RoundOutcome {
        let left_move = self.left.make_move();
        let right_move = self.right.make_move();
        self.left.add_opponent_move(right_move);
        self.right.add_opponent_move(left_move);
        match (left_move, right_move) {
            (Move::Cooperate, Move::Cooperate) => {
                self.left.modify_score(2);
                self.right.modify_score(2);
                RoundOutcome::BothCooperated
            }
            (Move::Cheat, Move::Cooperate) => {
                self.left.modify_score(3);
                self.right.modify_score(-1);
                RoundOutcome::LeftCheated
            }
            (Move::Cooperate, Move::Cheat) => {
                self.left.modify_score(-1);
                self.right.modify_score(3);
                RoundOutcome::RightCheated
            }
            (Move::Cheat, Move::Cheat) => RoundOutcome::BothCheated,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub trait Agent {
    // fn new() -> Self
    // where
    //     Self: Sized;
    fn score(&self) -> i32;
    fn make_move(&self) -> Move;
    fn add_opponent_move(&mut self, opponent_move: Move);
    fn modify_score(&mut self, plus: i32);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Move {
    Cooperate,
    Cheat,
}

struct Data {
    score: i32,
    opponent_moves: Vec<Move>,
}

impl Data {
    fn new() -> Self {
        Self {
            score: 0,
            opponent_moves: Vec::new(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct CheatingAgent {
    data: Data,
}
impl CheatingAgent {
    pub fn new() -> Self {
        Self { data: Data::new() }
    }
}
impl Default for CheatingAgent {
    fn default() -> Self {
        Self::new()
    }
}
impl Agent for CheatingAgent {
    fn score(&self) -> i32 {
        self.data.score
    }
    fn make_move(&self) -> Move {
        Move::Cheat
    }
    fn add_opponent_move(&mut self, opponent_move: Move) {
        self.data.opponent_moves.push(opponent_move);
    }
    fn modify_score(&mut self, plus: i32) {
        self.data.score += plus;
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct CooperatingAgent {
    data: Data,
}
impl CooperatingAgent {
    pub fn new() -> Self {
        Self { data: Data::new() }
    }
}
impl Default for CooperatingAgent {
    fn default() -> Self {
        Self::new()
    }
}
impl Agent for CooperatingAgent {
    fn score(&self) -> i32 {
        self.data.score
    }
    fn make_move(&self) -> Move {
        Move::Cooperate
    }
    fn add_opponent_move(&mut self, opponent_move: Move) {
        self.data.opponent_moves.push(opponent_move);
    }
    fn modify_score(&mut self, plus: i32) {
        self.data.score += plus;
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct GrudgerAgent {
    data: Data,
}
impl GrudgerAgent {
    pub fn new() -> Self {
        Self { data: Data::new() }
    }
}
impl Default for GrudgerAgent {
    fn default() -> Self {
        Self::new()
    }
}
impl Agent for GrudgerAgent {
    fn score(&self) -> i32 {
        self.data.score
    }
    fn make_move(&self) -> Move {
        if self.data.opponent_moves.contains(&Move::Cheat) {
            return Move::Cheat;
        }
        Move::Cooperate
    }
    fn add_opponent_move(&mut self, opponent_move: Move) {
        self.data.opponent_moves.push(opponent_move);
    }
    fn modify_score(&mut self, plus: i32) {
        self.data.score += plus;
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct CopycatAgent {
    data: Data,
}
impl CopycatAgent {
    pub fn new() -> Self {
        Self { data: Data::new() }
    }
}
impl Default for CopycatAgent {
    fn default() -> Self {
        Self::new()
    }
}
impl Agent for CopycatAgent {
    fn score(&self) -> i32 {
        self.data.score
    }
    fn make_move(&self) -> Move {
        *self.data.opponent_moves.last().unwrap_or(&Move::Cooperate)
    }
    fn add_opponent_move(&mut self, opponent_move: Move) {
        self.data.opponent_moves.push(opponent_move);
    }
    fn modify_score(&mut self, plus: i32) {
        self.data.score += plus;
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct DetectiveAgent {
    data: Data,
}
impl DetectiveAgent {
    pub fn new() -> Self {
        Self { data: Data::new() }
    }
}
impl Default for DetectiveAgent {
    fn default() -> Self {
        Self::new()
    }
}
impl Agent for DetectiveAgent {
    fn score(&self) -> i32 {
        self.data.score
    }
    fn make_move(&self) -> Move {
        const STRATEGY: [Move; 4] = [
            Move::Cooperate,
            Move::Cheat,
            Move::Cooperate,
            Move::Cooperate,
        ];

        let moves_len = self.data.opponent_moves.len();
        if moves_len < STRATEGY.len() {
            return STRATEGY[moves_len];
        }
        if !self.data.opponent_moves.contains(&Move::Cheat) {
            return Move::Cheat;
        }
        *self.data.opponent_moves.last().unwrap_or(&Move::Cooperate)
    }
    fn add_opponent_move(&mut self, opponent_move: Move) {
        self.data.opponent_moves.push(opponent_move);
    }
    fn modify_score(&mut self, plus: i32) {
        self.data.score += plus;
    }
}
