use std::cmp::Ordering;

/// The union find algorithm to get a partitioning of a set.
#[derive(Debug, Clone, Default)]
pub struct UnionFind<T>(Vec<(Option<T>, u32, u8)>);

impl<T> UnionFind<T> {
    pub fn new(len: usize, mut make_default: impl FnMut(u32) -> T) -> Self {
        let len = u32::try_from(len).expect("Length overflow");
        Self((0..len).map(|i| (Some(make_default(i)), i, 0)).collect())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push(&mut self, value: T) -> u32 {
        let i = self.len() as u32;
        let _ = i.checked_add(1).expect("Length overflow");
        self.0.push((Some(value), i, 0));
        i
    }

    pub fn get_mut(&mut self, x: u32) -> &mut T {
        let x = self.find(x);
        self.0[x as usize].0.as_mut().unwrap()
    }

    pub fn find(&mut self, x: u32) -> u32 {
        let mut to = self.0[x as usize].1;
        if to != x {
            to = self.find(to);
            self.0[x as usize].1 = to;
        }
        to
    }

    pub fn unify(&mut self, a: u32, b: u32) -> (u32, Option<T>) {
        let (a, b) = (self.find(a), self.find(b));
        if a == b {
            return (a, None);
        }

        let (a, b) = (a as usize, b as usize);
        let lhs = self.0[a].0.take().unwrap();
        let rhs = self.0[b].0.take().unwrap();
        let final_idx = match Ord::cmp(&self.0[a].2, &self.0[b].2) {
            Ordering::Less => {
                self.0[a].1 = b as u32;
                self.0[b].0 = Some(lhs);
                b
            }
            Ordering::Greater => {
                self.0[b].1 = a as u32;
                self.0[a].0 = Some(lhs);
                a
            }
            Ordering::Equal => {
                self.0[a].1 = b as u32;
                self.0[b].2 += 1;
                self.0[b].0 = Some(lhs);
                b
            }
        };
        (final_idx as u32, Some(rhs))
    }
}
