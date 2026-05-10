#![allow(unused)]
use memchr::{memchr, memchr2, memchr3};

pub struct Stream<'a> {
    text: &'a [u8]
}

impl<'a> Stream<'a> {
    /// If the stream starts with 'what', drop it from the stream and return 'true',
    /// else return 'false'.
    pub fn consume(&mut self, what: &[u8]) -> bool {
        if self.starts_with(what) {
            self.text = &self.text[what.len()..];
            true
        } else {
            false
        }
    }

    /// Check if this stream contains a byte.
    pub fn contains(&self, c: u8) -> bool { self.find_char(c).is_some() }

    /// Check if this stream contains a slice.
    pub fn contains_slice(&self, s: &[u8]) -> bool { self.find(s).is_some() }

    /// Count the number of instances that a byte appears in the stream.
    pub fn count(&self, c: u8) -> usize {
        let mut n = 0;
        let mut s = Stream::new(self.text);
        while let Some(idx) = s.find_char(c) {
            n += 1;
            s.take_until_idx(idx);
            s.drop_byte();
        }
        n
    }

    /// Drop the first byte from the stream.
    pub fn drop_byte(&mut self) -> &mut Self {
        self.text = &self.text[1..];
        self
    }

    /// Find the first index of a byte in the stream.
    pub fn find_char(&self, c: u8) -> Option<usize> { memchr(c, self.text) }

    /// Find the first index of a string in the stream.
    pub fn find(&self, text: &[u8]) -> Option<usize> { memchr::memmem::find(self.text, text) }

    /// Find the first index of any of a number of bytes.
    pub fn find_any(&self, what: &[u8]) -> Option<usize> {
        match what.len() {
            0 => None,
            1 => memchr(what[0], self.text),
            2 => memchr2(what[0], what[1], self.text),
            3 => memchr3(what[0], what[1], what[2], self.text),
            _ => self.text.iter().position(|&c| memchr(c, what).is_some())
        }
    }

    /// Find the index of the first character that *doesn't* match.
    pub fn find_first_not_of(&self, what: &[u8]) -> Option<usize> {
        self.text.iter().position(|c|!what.contains(c))
    }

    /// Get the first byte of the stream, if there is one.
    pub fn front(&self) -> Option<u8> { self.text.first().map(|&b|b) }

    /// Check if this stream is empty.
    pub fn is_empty(&self) -> bool { self.text.is_empty() }

    /// Make a new stream.
    pub fn new(text: &'a [u8]) -> Self { Stream { text } }

    /// Split the stream.
    pub fn split(&self, c: u8) -> impl Iterator<Item = &[u8]> {
        self.text.split(move |&b| b == c)
    }

    /// Split the stream at 'c' and return the two parts.
    pub fn split_once(&self, c: u8) -> Option<(&[u8], &[u8])> {
        self.find_char(c).map(|n| (&self.text[..n], &self.text[(n+1)..]))
    }

    /// Split once at a string.
    pub fn split_once_str(&self, s: &[u8]) -> Option<(&[u8], &[u8])> {
        self.find(s).map(|n| (&self.text[..n], &self.text[(n+s.len())..]))
    }

    /// Check if the stream starts with 'what'.
    pub fn starts_with(&self, what: &[u8]) -> bool { self.text.starts_with(what) }

    /// Check if this starts with any of the characters in 'what'.
    pub fn starts_with_any(&self, what: &[u8]) -> bool {
        !self.is_empty() && memchr(self.text[0], what).is_some()
    }

    /// Clear the stream and return its contents.
    pub fn take_all(&mut self) -> &'a [u8] {
        let ret = self.text;
        self.text = &self.text[self.text.len()..];
        ret
    }

    /// Remove the first byte from the stream and return it.
    pub fn take_byte(&mut self) -> Option<u8> {
        let c = self.front();
        if c.is_none() { None }
        else {
            self.text = &self.text[1..];
            c
        }
    }

    /// Find the first instance of 'what' in the stream, remove everything before
    /// it and return it; if 'what' is not found, returns the entire stream and clears it.
    pub fn take_until(&mut self, what: &[u8]) -> &'a [u8] {
        match self.find(what) {
            Some(idx) => self.take_until_idx(idx),
            None => self.take_all(),
        }
    }

    /// As take_until(), but also drops 'what'.
    pub fn take_until_and_drop(&mut self, what: &[u8]) -> &'a [u8] {
        let s = self.take_until(what);
        self.consume(what);
        s
    }

    /// As take_until() but uses the first occurrence of any of the characters in 'what' instead.
    pub fn take_until_any(&mut self, what: &[u8]) -> &'a [u8] {
        match self.find_any(what) {
            None => self.take_all(),
            Some(idx) => self.take_until_idx(idx)
        }
    }

    /// Take all characters up to (but not including) 'idx' and return them.
    pub fn take_until_idx(&mut self, idx: usize) -> &'a [u8] {
        let ret = &self.text[..idx];
        self.text = &self.text[idx..];
        ret
    }

    /// Take all characters until the first occurrence of 'a' or 'b' and return them.
    pub fn take_until_either(&mut self, a: &[u8], b: &[u8]) -> &'a [u8] {
        let a_idx = self.find(a);
        let b_idx = self.find(b);
        match (a_idx, b_idx) {
            (None, None) => self.take_all(),
            (None, Some(b)) => self.take_until_idx(b),
            (Some(a), None) => self.take_until_idx(a),
            (Some(a), Some(b)) => self.take_until_idx(a.min(b)),
        }
    }

    /// Take characters so long as they match a character in 'what'.
    pub fn take_while_any(&mut self, what: &[u8]) -> &'a [u8] {
        match self.find_first_not_of(what) {
            None => self.take_all(),
            Some(idx) => self.take_until_idx(idx)
        }
    }

    /// Get the contained text.
    pub fn text(&self) -> &'a [u8] { self.text }

    /// Trim whitespace on both ends.
    pub fn trim(&mut self) -> &mut Self {
        self.text = self.text.trim_ascii();
        self
    }

    /// Trim leading whitespace.
    pub fn trim_start(&mut self) -> &mut Self {
        self.text = self.text.trim_ascii_start();
        self
    }
}


