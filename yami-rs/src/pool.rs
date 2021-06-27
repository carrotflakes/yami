#[derive(Debug)]
pub struct StringPool {
    strings: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InternedString(*const u8);

impl StringPool {
    pub fn new() -> StringPool {
        StringPool {
            strings: Vec::new(),
        }
    }

    pub fn intern(&mut self, str: &str) -> InternedString {
        if let Some(i) = self.strings.iter().position(|x| *x == str) {
            InternedString(self.strings[i].as_str().as_ptr())
        } else {
            self.strings.push(str.to_string());
            InternedString(self.strings.last().unwrap().as_str().as_ptr())
        }
    }

    pub fn get(&self, is: InternedString) -> &String {
        self.strings
            .iter()
            .find(|x| x.as_str().as_ptr() == is.0)
            .unwrap()
    }
}

unsafe impl Send for InternedString {}
unsafe impl Sync for InternedString {}
