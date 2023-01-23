#[derive(Debug)]
pub struct Span<'a> {
    start: u32,
    end: u32,
    input: &'a str,
}

impl<'a> Span<'a> {
    pub fn new(start: u32, end: u32, input: &'a str) -> Self {
        Self { start, end, input }
    }

    pub fn input(&self) -> &str {
        self.input
    }

    pub fn subsliced_str(&self) -> &'a str {
        &self.input[(self.start as usize)..(self.end as usize)]
    }

    pub fn get_line_col(&self) -> (u32, u32) {
        self.input
            .chars()
            .take(self.start as usize)
            .fold((0, 0), |acc, ch| {
                if ch == '\n' {
                    (acc.0 + 1, 0)
                } else {
                    (acc.0, acc.1 + 1)
                }
            })
    }

    pub fn length(&self) -> u32 {
        self.end - self.start
    }
}
