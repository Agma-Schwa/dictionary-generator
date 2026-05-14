#![allow(unused)]

use std::fmt::Write;
use std::fmt::Debug;
use std::fmt::{Display, Formatter};
use std::ops::Range;
use ariadne::{Source, Span};
use memchr::{memchr, memchr2, memchr3};
use smallvec::{smallvec, SmallVec};

pub type RangeData = Range<usize>;
pub type SmallStr = super::NodeText;

/// Input file; used for diagnostics.
pub struct InputFile<'s> {
    pub contents: &'s str,
    pub name: &'s str,
}

/// A source location.
#[derive(Debug, Clone)]
pub struct SourceRange(pub Option<RangeData>);

/// Helper to map map offset ranges of a concatenated line back to the original lines.
///
/// This is a bit terrible, arguably, but the 'proper' way to to this, which is writing
/// a lexer, doesn't work too well because the parser is so highly context-dependent that
/// writing a separate lexer is basically a lost cause.
#[derive(Debug)]
pub struct SourceMap {
    pub ranges: SmallVec<[(usize, usize, SourceRange); 4]>
}

/// A stream.
#[derive(Debug, Clone)]
pub struct Stream<'text, 'map> {
    text: &'text [u8],
    source_map: &'map SourceMap
}

#[derive(Debug)]
pub enum SmallCow<'a> {
    Small(SmallStr),
    Std(String),
    Borrowed(&'a str),
}

impl<'a> SmallCow<'a> {
    pub fn as_bytes(&self) -> &[u8] {
        self.as_str().as_bytes()
    }

    pub fn as_str(&self) -> &str {
        match self {
            SmallCow::Small(s) => s.as_str(),
            SmallCow::Std(s) => s.as_str(),
            SmallCow::Borrowed(s) => s,
        }
    }

    pub fn clear(&mut self) {
        *self = SmallCow::Borrowed("");
    }

    pub fn into_small(self) -> SmallStr {
        match self {
           SmallCow::Small(s) => s,
           SmallCow::Std(s) => s.into(),
           SmallCow::Borrowed(s) => s.into(),
        }
    }
}


impl SourceRange {
    pub fn extend(&self, r: &SourceRange) -> SourceRange {
        match (&self.0, &r.0) {
            (Some(l), Some(r)) => {
                let start = l.start.min(r.start);
                let end = l.end.max(r.end);
                SourceRange(Some(start..end))
            },
            _ => SourceRange(None),
        }
    }

    pub fn offset(&self, n: usize) -> SourceRange {
        match &self.0 {
            Some(s) => SourceRange(Some((s.end.min(s.start + n)..s.end))),
            _ => SourceRange(None)
        }
    }

    pub fn take(&self, n: usize) -> SourceRange {
        match &self.0 {
            Some(s) => SourceRange(Some(s.start..s.end.min(s.start + n))),
            _ => SourceRange(None)
        }
    }
}

impl SourceMap {
    pub fn add(&mut self, chunk: &[u8], range: SourceRange) -> &mut Self {
        self.ranges.push((chunk.as_ptr().addr(), chunk.len(), range));
        self
    }

    fn find(&self, addr: usize, len: usize) -> SourceRange {
        fn range_in(in_addr: usize, in_len: usize, addr: usize, len: usize) -> Option<Range<usize>> {
            // '&[u8]::element_offset()' is annoying because it requires us to
            // pass a reference to an element, and '&text[0]' panics if the slice
            // is empty, so reimplement the parts we care about manually here.
            let offs = addr.wrapping_sub(in_addr);
            if offs < in_len {
                // Ensure we don't overflow.
                Some(offs..(offs + len).min(in_len))
            } else {
                None
            }
        }

        // Find the range that maps to that offset.
        let Some((range, physical)) = self.ranges.iter().find_map(|(in_addr, in_len, phys)|
            range_in(*in_addr, *in_len, addr, len).map(|x|(x, phys))
        ) else { return SourceRange(None) };

        // Finally map the offset to one in the actual range.
        if let Some(physical) = &physical.0 {
            let start = physical.start + range.start;
            SourceRange(Some((start..(start+len).min(physical.end))))
        } else {
            SourceRange(None)
        }
    }

    pub fn new() -> Self {
        Self { ranges: SmallVec::new() }
    }

    pub fn for_file(file: &InputFile) -> Self {
        let mut map = Self::new();
        map.add(file.contents.as_bytes(), SourceRange(Some(0..file.contents.len())));
        map
    }
}

/// Convert a byte slice to a str
pub fn make_str(s: &[u8]) -> &str {
    if cfg!(debug_assertions) {
        str::from_utf8(s).unwrap()
    } else {
        // Safety: All of our string parsing splits strings at ASCII characters, which
        // means that if the original string as UTF-8, so are any of the parts we split
        // off.
        unsafe { str::from_utf8_unchecked(s) }
    }
}

impl<'text, 'map> Stream<'text, 'map> {
    /// Get the address of the start of the stream.
    pub fn addr(&self) -> usize { self.text.as_ptr().addr() }

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
        let mut s = Stream::new(self.text, self.source_map);
        while let Some(idx) = s.find_char(c) {
            n += 1;
            s.take(idx);
            s.drop_byte();
        }
        n
    }

    /// Debug print this stream.
    pub fn debug(&self) -> String {
        let mut s = String::new();
        s.push_str("Stream(");
        write!(s, "\n  {{{:#x}, {:?}}},", self.text.as_ptr().addr(), make_str(self.text)).unwrap();
        for (addr, len, range) in &self.source_map.ranges {
            write!(
                s,
                "\n  ({:#x}, {:#x}, {}),",
                addr,
                len,
                match &range.0 {
                    Some(s) => format!("{}..{}", s.start, s.end),
                    None => "None".to_string()
                }
            ).unwrap();
        }
        s.push_str("\n)");
        s
    }

    /// Remove the first 'min(n, self.len())' bytes from the stream and return this.
    pub fn drop(&mut self, n: usize) -> &mut Self {
        self.take(n);
        self
    }

    /// Drop the first byte from the stream.
    pub fn drop_byte(&mut self) -> &mut Self {
        self.drop(1)
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

    /// Get the length of the stream.
    pub fn len(&self) -> usize { self.text.len() }

    /// Remove the first char from the stream and return it.
    pub fn measure_first_char(&mut self) -> Option<usize> {
        self.str().chars().next().map(|c|c.len_utf8())
    }

    /// Make a new stream.
    pub fn new(text: &'text [u8], source_map: &'map SourceMap) -> Self {
        Self { text, source_map }
    }

    /// Get the start offset of this stream.
    pub fn offs(&self) -> SourceRange {
        self.source_map.find(self.text.as_ptr().addr(), 1)
    }

    /// Get the source span of this stream.
    pub fn span(&self) -> SourceRange {
        self.source_map.find(self.text.as_ptr().addr(), self.len())
    }

    /// Split the stream.
    pub fn split(&self, c: u8) -> impl Iterator<Item = Self> {
        self.text.split(move |&b| b == c).map(|s| Stream::new(s, self.source_map))
    }

    /// Split the stream at 'c' and return the two parts.
    pub fn split_once(&self, c: u8) -> Option<(Self, Self)> {
        self.find_char(c).map(|n| (
            Stream::new(&self.text[..n], self.source_map),
            Stream::new(&self.text[(n+1)..], self.source_map),
        ))
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

    /// Get the text as a str.
    pub fn str(&self) -> &'text str { make_str(self.text()) }

    /// Clear the stream and return its contents.
    pub fn take_all(&mut self) -> Self { self.take(self.len()) }

    /// Remove the first 'min(n, self.len())' bytes from the stream and return them.
    pub fn take(&mut self, n: usize) -> Self {
        let n = n.min(self.len());
        let s = &self.text[..n];
        self.text = &self.text[n..];
        Self::new(s, self.source_map)
    }

    /// Remove the first byte from the stream and return it.
    pub fn take_byte(&mut self) -> Option<u8> {
        let s = self.take(1);
        if s.is_empty() { None }
        else { s.front() }
    }

    /// Find the first instance of 'what' in the stream, remove everything before
    /// it and return it; if 'what' is not found, returns the entire stream and clears it.
    pub fn take_until(&mut self, what: &[u8]) -> Self {
        match self.find(what) {
            Some(idx) => self.take(idx),
            None => self.take_all(),
        }
    }

    /// As take_until(), but also drops 'what'.
    pub fn take_until_and_drop(&mut self, what: &[u8]) -> Self {
        let s = self.take_until(what);
        self.consume(what);
        s
    }

    /// As take_until() but uses the first occurrence of any of the characters in 'what' instead.
    pub fn take_until_any(&mut self, what: &[u8]) -> Self {
        match self.find_any(what) {
            None => self.take_all(),
            Some(idx) => self.take(idx)
        }
    }

    /// Take all characters until the first occurrence of 'a' or 'b' and return them.
    pub fn take_until_either(&mut self, a: &[u8], b: &[u8]) -> Self {
        let a_idx = self.find(a);
        let b_idx = self.find(b);
        match (a_idx, b_idx) {
            (None, None) => self.take_all(),
            (None, Some(b)) => self.take(b),
            (Some(a), None) => self.take(a),
            (Some(a), Some(b)) => self.take(a.min(b)),
        }
    }

    /// Take characters so long as they match a character in 'what'.
    pub fn take_while_any(&mut self, what: &[u8]) -> Self {
        match self.find_first_not_of(what) {
            None => self.take_all(),
            Some(idx) => self.take(idx)
        }
    }

    /// Get the contained text.
    pub fn text(&self) -> &'text [u8] { self.text }

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

impl AsRef<[u8]> for Stream<'_, '_> {
    fn as_ref(&self) -> &[u8] { &self.text }
}

impl Display for Stream<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.str(), f)
    }
}
