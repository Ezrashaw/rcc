use std::collections::VecDeque;

pub struct PeekableFar<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    peeked: VecDeque<Option<I::Item>>,
}

impl<I: Iterator> PeekableFar<I> {
    pub fn new(iter: I) -> PeekableFar<I> {
        PeekableFar {
            iter,
            peeked: VecDeque::new(),
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&I::Item> {
        self.peek_far(1)
    }

    #[inline]
    pub fn peek_far(&mut self, d: usize) -> Option<&I::Item> {
        if self.peeked.len() < d {
            for _ in 0..d - self.peeked.len() {
                self.peeked.push_back(self.iter.next());
            }
        }

        self.peeked[d - 1].as_ref()
    }
}

impl<I: Iterator> Iterator for PeekableFar<I> {
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        if self.peeked.len() == 0 {
            self.iter.next()
        } else {
            self.peeked.pop_front().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {}
