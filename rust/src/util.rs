//! Utility functions from [Util.hs](../Util.hs)
//!
//! Ported from: Util.hs

// Ported from: Util.hs :: swap
/// Swap two elements in a list by index (infinite list version)
pub fn swap<T: Clone>(as_list: &[T], n: usize, m: usize) -> Vec<T> {
    let mut result = as_list.to_vec();
    if n < result.len() && m < result.len() {
        result.swap(n, m);
    }
    result
}

// Ported from: Util.hs :: swapFinite
/// Swap two elements in a finite list by index
pub fn swap_finite<T: Clone>(as_list: &[T], n: usize, m: usize) -> Vec<T> {
    swap(as_list, n, m)
}

// Ported from: Util.hs :: swapFiniteWithDefault
/// Swap two elements in a list, extending with default if needed
pub fn swap_finite_with_default<T: Clone>(def: T, ts: &[T], n: usize, m: usize) -> Vec<T> {
    let max_idx = n.max(m) + 1;
    let target_len = max_idx.max(ts.len());

    let mut extended: Vec<T> = ts.to_vec();
    while extended.len() < target_len {
        extended.push(def.clone());
    }

    if n < extended.len() && m < extended.len() {
        extended.swap(n, m);
    }

    extended
}
