
pub enum MilaErr {
    ReadStdInFailed(std::io::Error),
    EOFReached{line: u32, col: u32},
    UnexpectedChar{c: char, line: u32, col: u32},
    UnexpectedNumberEnd{c: char, line: u32, col: u32},
}

