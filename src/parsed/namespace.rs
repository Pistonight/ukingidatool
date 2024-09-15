use std::borrow::Cow;

#[derive(Debug, Default, Clone)]
pub struct Namespace<'a>(Vec<Cow<'a, str>>);
impl<'a> Namespace<'a> {
    pub fn push(&mut self, name: impl Into<Cow<'a, str>>) {
        self.0.push(name.into());
    }

    pub fn pop(&mut self) {
        if self.0.pop().is_none() {
            panic!("Namespace::pop() called on empty stack");
        }
    }

    pub fn get_with(&self, name: &str) -> String {
        let mut stack = self.0.clone();
        stack.push(Cow::Borrowed(name));
        stack.join("::")
    }

    fn get(&self) -> String {
        self.0.join("::")
    }

}
