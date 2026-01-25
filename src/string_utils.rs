// Rust string parsing libraries are stupid so I have to write this garbage instead.

pub struct Stream<'a> {
    text: &'a str
}

impl<'a> Stream<'a> {
    pub fn consume(&mut self, what: &str) -> bool {
        if self.starts_with(what) {
            self.text = &self.text[what.len()..];
            true
        } else {
            false
        }
    }

    pub fn drop_byte(&mut self) -> &mut Self {
        self.text = &self.text[1..];
        self
    }

    pub fn front(&self) -> Option<char> {
        self.text.chars().nth(0)
    }

    pub fn is_empty(&self) -> bool { self.text.is_empty() }

    pub fn new(s: &'a str) -> Self {
        Stream { text: s}
    }

    pub fn starts_with(&self, what: &str) -> bool {
        self.text.starts_with(what)
    }

    pub fn take_all(&mut self) -> &'a str {
        let ret = self.text;
        self.text = "";
        ret
    }

    pub fn take_byte(&mut self) -> Option<char> {
        let c = self.front();
        if c.is_none() { None }
        else {
            self.text = &self.text[1..];
            c
        }
    }

    pub fn take_until(&mut self, what: &str) -> &'a str {
        match self.text.find(what) {
            None => self.take_all(),
            Some(idx) => self.take_until_idx(idx)
        }
    }

    pub fn take_until_any(&mut self, what: &[char]) -> &'a str {
        match self.text.find(what) {
            None => self.take_all(),
            Some(idx) => self.take_until_idx(idx)
        }
    }

    pub fn take_until_idx(&mut self, idx: usize) -> &'a str {
        let ret = &self.text[0..idx];
        self.text = &self.text[idx..];
        ret
    }

    pub fn take_until_either(&mut self, a: &str, b: &str) -> &'a str {
        let a_idx = self.text.find(a);
        let b_idx = self.text.find(b);
        match (a_idx, b_idx) {
            (None, None) => self.take_all(),
            (None, Some(b)) => self.take_until_idx(b),
            (Some(a), None) => self.take_until_idx(a),
            (Some(a), Some(b)) => self.take_until_idx(a.min(b)),
        }
    }

    pub fn text(&self) -> &'a str { self.text }

    pub fn trim(&mut self) -> &mut Self {
        self.text = self.text.trim();
        self
    }

    pub fn trim_start(&mut self) -> &mut Self {
        self.text = self.text.trim_start();
        self
    }
}


