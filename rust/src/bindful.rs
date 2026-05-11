//! Monad for binding values to numbered variables
//!
//! Ported from: Bindful.hs
//!
//! This module provides a convenient abstraction for managing numbered variable
//! bindings during semantic analysis. It handles:
//! - Binding values to numbered variables
//! - Finding the next free variable number
//! - Scoped bindings with automatic cleanup
//! - Modifying all bound values
//!
//! Original Haskell:
//! ```haskell
//! class Monad m => BindfulMonad s m | m -> s where
//!     withBinding :: s -> (Int -> m r) -> m r
//!     binding :: Int -> m s
//!     twiddleBound :: (s -> s) -> m ()
//!     getValues :: m [s]
//!     evalBindful :: m a -> a
//! ```

use std::collections::HashMap;

// Ported from: Bindful.hs :: Bindful
/// Bindful state: maps variable numbers to values
#[derive(Debug, Clone)]
pub struct Bindful<S> {
    bindings: HashMap<i32, S>,
}

impl<S: Clone> Bindful<S> {
    // Ported from: Bindful.hs :: evalBindful
    /// Create a new empty Bindful state
    pub fn new() -> Self {
        Bindful {
            bindings: HashMap::new(),
        }
    }

    // Ported from: Bindful.hs :: nextFree
    /// Find the next free variable number
    pub fn next_free(&self) -> i32 {
        (1..)
            .find(|n| !self.bindings.contains_key(n))
            .unwrap_or(1)
    }

    // Ported from: Bindful.hs :: bind
    /// Bind a value to a variable number
    pub fn bind(&mut self, n: i32, value: S) {
        self.bindings.insert(n, value);
    }

    // Ported from: Bindful.hs :: unbind
    /// Unbind a variable number
    pub fn unbind(&mut self, n: i32) {
        self.bindings.remove(&n);
    }

    // Ported from: Bindful.hs :: binding
    /// Get the value bound to a variable number
    pub fn binding(&self, n: i32) -> Option<S> {
        self.bindings.get(&n).cloned()
    }

    // Ported from: Bindful.hs :: getValues
    /// Get all bound values
    pub fn get_values(&self) -> Vec<S> {
        self.bindings.values().cloned().collect()
    }

    // Ported from: Bindful.hs :: twiddleBound
    /// Apply a function to all bound values
    pub fn twiddle_bound<F>(&mut self, f: F)
    where
        F: Fn(&S) -> S,
    {
        let keys: Vec<i32> = self.bindings.keys().cloned().collect();
        for n in keys {
            if let Some(value) = self.bindings.get(&n).cloned() {
                self.bindings.insert(n, f(&value));
            }
        }
    }

    // Ported from: Bindful.hs :: withBinding
    /// Execute a function with a temporary binding, then unbind
    pub fn with_binding<R, F>(&mut self, value: S, f: F) -> R
    where
        F: FnOnce(i32, &mut Self) -> R,
    {
        let n = self.next_free();
        self.bind(n, value);
        let result = f(n, self);
        self.unbind(n);
        result
    }

    // Ported from: Bindful.hs :: bindNext
    /// Bind a value to the next free variable number
    pub fn bind_next(&mut self, value: S) -> i32 {
        let n = self.next_free();
        self.bind(n, value);
        n
    }
}

impl<S: Clone> Default for Bindful<S> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_free() {
        let bf: Bindful<String> = Bindful::new();
        assert_eq!(bf.next_free(), 1);
    }

    #[test]
    fn test_bind_and_binding() {
        let mut bf = Bindful::new();
        bf.bind(1, "hello".to_string());
        assert_eq!(bf.binding(1), Some("hello".to_string()));
        assert_eq!(bf.binding(2), None);
    }

    #[test]
    fn test_unbind() {
        let mut bf = Bindful::new();
        bf.bind(1, "hello".to_string());
        bf.unbind(1);
        assert_eq!(bf.binding(1), None);
    }

    #[test]
    fn test_with_binding() {
        let mut bf = Bindful::new();
        let result = bf.with_binding("temp".to_string(), |n, bf| {
            assert_eq!(bf.binding(n), Some("temp".to_string()));
            42
        });
        assert_eq!(result, 42);
        assert_eq!(bf.binding(1), None); // Should be unbound after
    }

    #[test]
    fn test_twiddle_bound() {
        let mut bf = Bindful::new();
        bf.bind(1, 10);
        bf.bind(2, 20);
        bf.twiddle_bound(|x| x + 5);
        assert_eq!(bf.binding(1), Some(15));
        assert_eq!(bf.binding(2), Some(25));
    }

    #[test]
    fn test_get_values() {
        let mut bf = Bindful::new();
        bf.bind(1, "a".to_string());
        bf.bind(2, "b".to_string());
        let mut values = bf.get_values();
        values.sort();
        assert_eq!(values, vec!["a".to_string(), "b".to_string()]);
    }
}
