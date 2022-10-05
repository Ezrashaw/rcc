use std::collections::VecDeque;

pub struct PeekableFar<I: Iterator> {
    iter: I,
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
        if self.peeked.is_empty() {
            self.iter.next()
        } else {
            self.peeked.pop_front().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::PeekableFar;

    #[test]
    pub fn std_lib_peekable() {
        let xs = [1, 2, 3];

        let mut iter = PeekableFar::new(xs.iter());

        // `peek` lets us see into the future
        assert_eq!(iter.peek(), Some(&&1));
        assert_eq!(iter.next(), Some(&1));

        assert_eq!(iter.next(), Some(&2));

        // we can `peek` multiple times, the iterator won't advance
        assert_eq!(iter.peek(), Some(&&3));
        assert_eq!(iter.peek(), Some(&&3));

        assert_eq!(iter.next(), Some(&3));

        // after the iterator is finished, so is `peek`
        assert_eq!(iter.peek(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    pub fn far_peekable() {
        let xs = [1, 2, 3];

        let mut iter = PeekableFar::new(xs.iter());

        // `peek_far` lets us see futher into the future
        assert_eq!(iter.peek_far(2), Some(&&2));
        assert_eq!(iter.peek_far(1), Some(&&1));

        // the iterator hasn't advanced and isn't affected by peeking far
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));

        // `peek` is equal to `peek_far(1)` and works just as fine
        assert_eq!(iter.peek(), Some(&&3));
        assert_eq!(iter.peek_far(1), Some(&&3));

        assert_eq!(iter.next(), Some(&3));

        // `peek_far` returns `None` for out-of-bound values
        assert_eq!(iter.peek_far(100), None);
        // and when the iterator is finished
        assert_eq!(iter.peek_far(1), None);
        assert_eq!(iter.next(), None);
    }
}
