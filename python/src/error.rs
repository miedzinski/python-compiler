use std::error;

use inkwell::support::LLVMString;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub(crate) struct LlvmError(String);

impl From<LLVMString> for LlvmError {
    fn from(e: LLVMString) -> LlvmError {
        LlvmError(e.to_string())
    }
}

impl Display for LlvmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl error::Error for LlvmError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}
